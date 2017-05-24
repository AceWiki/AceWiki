package ch.uzh.ifi.attempto.chartparser;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.base.Joiner;
import com.google.common.base.Strings;

public class SemanticTreeProcessor {

	private final static String lamFunctor = "lam";
	private final static String appFunctor = "app";
	private final static String concatFunctor = "+";

	private final RootNode rootNode;

	public SemanticTreeProcessor(ParseTree parseTree) {
		this.rootNode = buildTree(checkNotNull(parseTree));
	}

	private RootNode buildTree(ParseTree parseTree) {
		Object parseTreeNode = parseTree.getSemTree();
		RootNode root = new RootNode();
		recursiveBuildTree(parseTreeNode, root);
		return root;
	}

	public RootNode reduce() {
		this.rootNode.accept(new Visitor());
		return this.rootNode;
	}

	private void recursiveBuildTree(Object nodeObj, AbstractNode parent) {
		AbstractNode node = null;
		if (nodeObj instanceof Object[]) {
			Object[] array = (Object[]) nodeObj;

			checkArgument(array.length >= 3, "array length < 3: " + array);
			checkArgument(array[0] instanceof String, "array[0] is not a string: " + array[0]);

			String functor = (String) array[0];
			switch (functor) {
			case lamFunctor:
				node = new Lambda(parent);
				break;
			case appFunctor:
				node = new App(parent);
				break;
			case concatFunctor:
				node = new Concat(parent);
				break;
			default:
				throw new IllegalArgumentException("unknown functor: " + functor);
			}

			for (int i = 1; i < array.length; i++) {
				recursiveBuildTree(array[i], node);
			}
		} else if (nodeObj instanceof String) {
			node = new StringNode(parent, (String) nodeObj);
		} else if (nodeObj instanceof StringObject) {
			node = new StringNode(parent, (StringObject) nodeObj);
		} else if (nodeObj instanceof StringRef) {
			node = new VariableNode(parent, (StringRef) nodeObj);
		}

		checkArgument(node != null, "Unknown node type: " + nodeObj);
		parent.getChildren().add(node);
	}

	public static class Visitor {

		private final Map<String, String> variableValueMap = new HashMap<>();

		public void visit(App app) {
			if (app.getLeft().isLambda() && app.getRight().isString()) {
				VariableNode varNode = app.getLambda().getVariable();
				StringNode stringNode = app.getString();
				variableValueMap.put(varNode.getVarName(), stringNode.getText());
				AbstractNode result = app.getLambda().apply(variableValueMap);
				AbstractNode parent = app.getParent();
				parent.replaceChild(app, result);
				parent.accept(this);
			} else {
				defaultVisit(app);
			}
		}

		public void visit(Concat concat) {
			if (concat.allChildrenAreStringNodes()) {
				AbstractNode newChild = new StringNode(concat.getParent(), concat.concatenate());
				concat.getParent().replaceChild(concat, newChild);
				concat.getParent().accept(this);
			} else {
				defaultVisit(concat);
			}
		}

		public void visit(AbstractNode node) {
			defaultVisit(node);
		}

		private void defaultVisit(AbstractNode node) {
			for (AbstractNode child : node.getChildren()) {
				child.accept(this);
			}
		}
	}

	public static abstract class AbstractNode {

		private AbstractNode parent;
		private final List<AbstractNode> children = new ArrayList<>();
		protected final int depth;

		public AbstractNode(AbstractNode parent, int depth) {
			this.parent = parent;
			this.depth = depth;
		}

		public AbstractNode(AbstractNode parentNode) {
			this(checkNotNull(parentNode), parentNode.depth + 1);
		}

		@SuppressWarnings("unchecked")
		public <T extends AbstractNode> T cast() {
			return (T) this;
		}

		public abstract void accept(Visitor visitor);

		public AbstractNode getParent() {
			return checkNotNull(parent, "parent node is null");
		}

		public int getDepth() {
			return depth;
		}

		public boolean isLambda() {
			return false;
		}

		public boolean isString() {
			return false;
		}

		public boolean isVariable() {
			return false;
		}

		public List<AbstractNode> getChildren() {
			return children;
		}

		public void replaceChild(AbstractNode oldChild, AbstractNode newChild) {
			int index = this.children.indexOf(oldChild);
			if (index != -1) {
				this.children.set(index, newChild);
				newChild.parent = this;
			}
		}

		protected String getIndent() {
			return "\n" + Strings.repeat(" ", this.depth);
		}

		@Override
		public String toString() {
			return getIndent() + getClass().getSimpleName().toLowerCase() + " (" + Joiner.on(", ").join(children) + ")";
		}

	}

	public static class RootNode extends AbstractNode {

		public RootNode() {
			super(null, 0);
		}

		public boolean isFullyReduced() {
			return this.getChildren().size() == 1 && this.getChildren().get(0).isString();
		}

		public String getSemantics() {
			String semantics;
			if (isFullyReduced()) {
				semantics = "'" + this.getChildren().get(0).<StringNode>cast().getText() + "'";
			} else {
				semantics = this.toString();
			}
			return semantics;
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.visit(this);
		}

		@Override
		public String toString() {
			return Joiner.on(", ").join(getChildren());
		}
	}

	public static class StringNode extends AbstractNode {

		private final String text;

		public StringNode(AbstractNode parent, String text) {
			super(parent);
			this.text = checkNotNull(text, "text is null");
		}

		public StringNode(AbstractNode parent, StringObject stringObject) {
			super(parent);
			this.text = checkNotNull(stringObject, "text is null").getString();
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.visit(this);
		}

		@Override
		public boolean isString() {
			return true;
		}

		public String getText() {
			return text;
		}

		@Override
		public String toString() {
			return getIndent() + "\"" + text + "\"";
		}
	}

	public static class VariableNode extends AbstractNode {

		private final StringRef stringRef;

		public VariableNode(AbstractNode parent, StringRef stringRef) {
			super(parent);
			this.stringRef = checkNotNull(stringRef);
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.visit(this);
		}

		@Override
		public boolean isVariable() {
			return true;
		}

		public StringRef getStringRef() {
			return stringRef;
		}

		public String getVarName() {
			return "v" + stringRef.getID();
		}

		@Override
		public String toString() {
			return getIndent() + getVarName();
		}

		public StringNode resolve(String value) {
			return new StringNode(getParent(), value);
		}
	}

	public static class Lambda extends AbstractNode {

		public Lambda(AbstractNode parent) {
			super(parent);
		}

		@Override
		public boolean isLambda() {
			return true;
		}

		public AbstractNode apply(Map<String, String> variableValueMap) {
			recursiveReplaceVariables(this.getRight(), variableValueMap);
			return this.getRight();
		}

		private void recursiveReplaceVariables(AbstractNode node, final Map<String, String> variableValueMap) {
			if (node.isVariable()) {
				VariableNode varNode = (VariableNode) node;
				String varName = varNode.getVarName();
				if (variableValueMap.containsKey(varName)) {
					node.getParent().replaceChild(node, varNode.resolve(variableValueMap.get(varName)));
				}
			} else {
				for (AbstractNode child : node.getChildren()) {
					recursiveReplaceVariables(child, variableValueMap);
				}
			}
		}

		public AbstractNode getLeft() {
			checkState(getChildren().size() == 2, "lambda expression without children");
			return this.getChildren().get(0);
		}

		public VariableNode getVariable() {
			return this.getLeft().cast();
		}

		public AbstractNode getRight() {
			checkState(getChildren().size() == 2, "lambda expression without children");
			return this.getChildren().get(1);
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.visit(this);
		}
	}

	public static class App extends AbstractNode {

		public App(AbstractNode parent) {
			super(parent);
		}

		public AbstractNode getLeft() {
			checkState(getChildren().size() == 2, "lambda application without children");
			return this.getChildren().get(0);
		}

		public Lambda getLambda() {
			checkState(getLeft().isLambda(), "lambda expression expected for lambda application");
			return this.getLeft().cast();
		}

		public AbstractNode getRight() {
			checkState(getChildren().size() == 2, "lambda application without children");
			return this.getChildren().get(1);
		}

		public StringNode getString() {
			checkState(getLeft().isLambda(), "string expected for lambda application");
			return this.getRight().cast();
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.visit(this);
		}
	}

	public static class Concat extends AbstractNode {

		public Concat(AbstractNode parent) {
			super(parent);
		}

		public boolean allChildrenAreStringNodes() {
			for (AbstractNode child : this.getChildren()) {
				if (!child.isString()) {
					return false;
				}
			}

			return true;
		}

		public String concatenate() {
			String concatResult = "";
			for (AbstractNode child : this.getChildren()) {
				checkState(child.isString(), "not all concat children are strings");
				concatResult += child.<StringNode>cast().getText();
			}
			return concatResult;
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.visit(this);
		}
	}

}

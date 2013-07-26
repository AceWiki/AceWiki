// This file is part of AceWiki.
// Copyright 2008-2013, AceWiki developers.
// 
// AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.echocomp;

import java.util.HashMap;
import java.util.Map;

import nextapp.echo.app.ApplicationInstance;
import nextapp.echo.app.Column;
import nextapp.echo.app.Component;
import nextapp.echo.app.TaskQueueHandle;

/**
 * This abstract class can be used to create components that are initialized asynchronously in the background.
 * This is makes sense for components that require some time to create themselves (e.g. because of time
 * expensive calculations). The possibly time consuming creation of the component can be done in a synchronized
 * way so that at most one such creation process (per application instance) is running at a time.
 * 
 * @author Tobias Kuhn
 */
public abstract class DelayedComponent extends Column {
	
	private static final long serialVersionUID = -3479950976442367130L;
	
	private static Map<String, TaskQueueHandle> taskQueues =
		new HashMap<String, TaskQueueHandle>();
	
	/**
	 * Creates a new delayed component which shows the temporary component until the real component is
	 * ready.
	 * 
	 * @param tempComponent The temporary component.
	 * @param synchronize Defines whether the calculation should be performed in a synchronized way.
	 */
	public DelayedComponent(Component tempComponent, boolean synchronize) {
		if (tempComponent != null) {
			add(tempComponent);
		}
		
		final ApplicationInstance application = EchoThread.getActiveApplication();
		TaskQueueHandle taskQueueTemp = taskQueues.get(application.toString());
		if (taskQueueTemp == null) {
			taskQueueTemp = application.createTaskQueue();
			taskQueues.put(application.toString(), taskQueueTemp);
		}
		final TaskQueueHandle taskQueue = taskQueueTemp;
		
		if (synchronize) {
			
			EchoThread thread = new EchoThread() {
				
				public ApplicationInstance getApplication() {
					return application;
				}
				
				public void run() {
					synchronized (application) {
						final Component c = initComponent();
						application.enqueueTask(
							taskQueue,
							new Runnable() {
								public void run() {
									DelayedComponent.this.removeAll();
									DelayedComponent.this.add(c);
									finalizeAction();
								}
							}
						);
						try {
							sleep(500);
						} catch (InterruptedException ex) {}
					}
				}
				
			};
			thread.setPriority(Thread.MIN_PRIORITY);
			thread.start();
			
		} else {
			
			EchoThread thread = new EchoThread() {
				
				public ApplicationInstance getApplication() {
					return application;
				}
				
				public void run() {
					final Component c = initComponent();
					application.enqueueTask(
						taskQueue,
						new Runnable() {
							public void run() {
								DelayedComponent.this.removeAll();
								DelayedComponent.this.add(c);
								finalizeAction();
							}
						}
					);
				}
				
			};
			thread.start();
			
		}
		
	}
	
	
	/**
	 * Creates a new delayed component with the given temporary component. The calculation is
	 * not synchronized.
	 * 
	 * @param tempComponent The temporary component.
	 */
	public DelayedComponent(Component tempComponent) {
		this(tempComponent, false);
	}
	
	
	/**
	 * Creates a new delayed component with no temporary component.
	 * 
	 * @param synchronize Defines whether the calculation should be performed in a synchronized way.
	 */
	public DelayedComponent(boolean synchronize) {
		this(null, synchronize);
	}
	
	
	/**
	 * Creates a new delayed component with no temporary component. The calculation is
	 * not synchronized.
	 */
	public DelayedComponent() {
		this(null, false);
	}
	
	/**
	 * This method should contain the (possibly time-consuming) operations to create the actual GUI
	 * component. This operation will be performed asynchronously. As soon as it is finished, the
	 * temporary component (if present) is replaced by the component this method returns.
	 * 
	 * @return The GUI component.
	 */
	public abstract Component initComponent();
	
	/**
	 * Override this method to run code in the appliction context (e.g. for GUI changes) after the
	 * asynchronous operation has finished.
	 */
	public void finalizeAction() {
	}

}

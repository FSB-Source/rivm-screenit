
package nl.rivm.screenit.main.web.component.pingpong;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.IFormModelUpdateListener;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class PingPongInput<T> extends GenericPanel<List<T>> implements IFormModelUpdateListener
{

	private static final long serialVersionUID = 1L;

	private static final String SELECTEDSUPPLEMENT = "1";

	private static final String NOTSELECTEDSUPPLEMENT = "2";

	private final IModel<List<T>> choices;

	private final IChoiceRenderer<T> choiceRenderer;

	private final PingPongRecoder<T> pingPongRecoder;

	public PingPongInput(String id, IModel<List<T>> model, IModel<List<T>> choices, IChoiceRenderer<T> choiceRenderer)
	{
		this(id, model, choices, choiceRenderer, false);
	}

	public PingPongInput(String id, IModel<List<T>> model, IModel<List<T>> choices, IChoiceRenderer<T> choiceRenderer, final boolean inzien)
	{
		super(id, model);

		this.choiceRenderer = choiceRenderer;
		this.choices = choices;

		pingPongRecoder = new PingPongRecoder<>("recorder", this);
		add(pingPongRecoder);

		add(new ListView<T>("selectedRows", choices)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<T> item)
			{
				StringBuilder javaScript = new StringBuilder();
				javaScript.append("$('#");
				javaScript.append(getNotSelectedId(item.getModel()));
				javaScript.append("').removeClass('display-none')");
				javaScript.append(".addClass('display-inline-block');");
				javaScript.append("$('#");
				javaScript.append(getSelectedId(item.getModel()));
				javaScript.append("').removeClass('display-inline-block')");
				javaScript.append(".addClass('display-none');");

				javaScript.append("var value = $('#");
				javaScript.append(pingPongRecoder.getMarkupId());
				javaScript.append("').val();");

				javaScript.append("value = value.replace($('#");
				javaScript.append(getSelectedId(item.getModel()));
				javaScript.append("').attr('data-value'), '');");

				javaScript.append("$('#");
				javaScript.append(pingPongRecoder.getMarkupId());
				javaScript.append("').val(value);");

				WebMarkupContainer container = new WebMarkupContainer("container");
				container.setOutputMarkupId(true);
				container.setMarkupId(PingPongInput.this.choiceRenderer.getIdValue(item.getModelObject(), -1) + SELECTEDSUPPLEMENT);

				container.add(new Label("label", PingPongInput.this.choiceRenderer.getDisplayValue(item.getModelObject()).toString()));

				container.add(new AttributeAppender("data-value", PingPongInput.this.choiceRenderer.getIdValue(item.getModelObject(), -1)));

				if (inzien)
				{
					container.add(new AttributeAppender("disabled", " disabled"));
				}
				else
				{
					containerOnClickEvent(javaScript, container);
				}
				if (!isSelected(item.getModelObject()))
				{
					container.add(new AttributeAppender("class", " display-none"));
				}

				item.add(container);

			}
		});

		add(new ListView<T>("notSelectedRows", choices)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<T> item)
			{
				StringBuilder javaScript = new StringBuilder();
				javaScript.append("$('#");
				javaScript.append(getSelectedId(item.getModel()));
				javaScript.append("').removeClass('display-none')");
				javaScript.append(".addClass('display-inline-block');");
				javaScript.append("$('#");
				javaScript.append(getNotSelectedId(item.getModel()));
				javaScript.append("').removeClass('display-inline-block')");
				javaScript.append(".addClass('display-none');");

				javaScript.append("var value = $('#");
				javaScript.append(pingPongRecoder.getMarkupId());
				javaScript.append("').val();");

				javaScript.append("value += ',' + $('#");
				javaScript.append(getSelectedId(item.getModel()));
				javaScript.append("').attr('data-value');");

				javaScript.append("$('#");
				javaScript.append(pingPongRecoder.getMarkupId());
				javaScript.append("').val(value);");
				javaScript.append("return false;");

				WebMarkupContainer container = new WebMarkupContainer("container");
				container.setOutputMarkupId(true);
				container.setMarkupId(PingPongInput.this.choiceRenderer.getIdValue(item.getModelObject(), -1) + NOTSELECTEDSUPPLEMENT);

				container.add(new Label("label", PingPongInput.this.choiceRenderer.getDisplayValue(item.getModelObject()).toString()));

				if (inzien)
				{
					container.add(new AttributeAppender("disabled", " disabled"));
				}
				else
				{
					containerOnClickEvent(javaScript, container);
				}
				if (isSelected(item.getModelObject()))
				{
					container.add(new AttributeAppender("class", " display-none"));
				}

				item.add(container);

			}
		});
	}

	private void containerOnClickEvent(StringBuilder javaScript, WebMarkupContainer container)
	{
		container.add(new AjaxEventBehavior("click")
		{
			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				attributes.getAjaxCallListeners().add(new AjaxCallListener()
				{
					@Override
					public CharSequence getPrecondition(Component component)
					{
						return javaScript;
					}
				});
			}

			@Override
			protected void onEvent(AjaxRequestTarget target)
			{

			}

		});
	}

	@Override
	public void updateModel()
	{

		Iterator<T> it = pingPongRecoder.getSelectedChoices();

		modelChanging();

		Collection<T> collection = getModelCollection();
		collection.clear();
		while (it.hasNext())
		{
			collection.add(it.next());
		}

		modelChanged();

		@SuppressWarnings("unchecked")
		IModel<Object> defaultModel = (IModel<Object>) getDefaultModel();
		defaultModel.setObject(collection);
	}

	public abstract IModel<T> model(T object);

	public IChoiceRenderer<T> getChoiceRenderer()
	{
		return choiceRenderer;
	}

	@SuppressWarnings("unchecked")
	public Collection<T> getModelCollection()
	{
		return (Collection<T>) getDefaultModelObject();
	}

	public IModel<List<T>> getChoices()
	{
		return choices;
	}

	private boolean isSelected(T object)
	{
		for (T selectedObject : getModelCollection())
		{
			if (object.equals(selectedObject))
			{
				return true;
			}
		}

		return false;
	}

	private String getNotSelectedId(IModel<T> rowModel)
	{
		return choiceRenderer.getIdValue(rowModel.getObject(), -1) + NOTSELECTEDSUPPLEMENT;
	}

	private String getSelectedId(IModel<T> rowModel)
	{
		return choiceRenderer.getIdValue(rowModel.getObject(), -1) + SELECTEDSUPPLEMENT;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(choiceRenderer);
	}
}

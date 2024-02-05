package nl.rivm.screenit.main.web.component;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.util.lang.Objects;

public class AjaxButtonGroup<T> extends RepeatingView
{

	private static final long serialVersionUID = 1L;

	private IModel<? extends List<T>> optionsModel;

	private IChoiceRenderer<T> choiceRenderer;

	public AjaxButtonGroup(String id, IModel<? extends List<T>> optionsModel, IChoiceRenderer<T> choiceRenderer)
	{
		this(id, null, optionsModel, choiceRenderer);
	}

	public AjaxButtonGroup(String id, IModel<T> selectionModel, IModel<? extends List<T>> optionsModel, IChoiceRenderer<T> choiceRenderer)
	{
		super(id, selectionModel);
		this.optionsModel = optionsModel;
		this.choiceRenderer = choiceRenderer;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		List<T> items = optionsModel.getObject();

		for (int ix = 0; ix < items.size(); ix++)
		{
			final int index = ix;
			AjaxLink<String> button = createButton(newChildId(), new LoadableDetachableModel<T>()
			{
				@Override
				protected T load()
				{
					return AjaxButtonGroup.this.optionsModel.getObject().get(index);
				}
			});

			add(button);
		}
	}

	protected AjaxLink<String> createButton(String id, final IModel<T> model)
	{
		return new IndicatingAjaxLink<String>(id, new LoadableDetachableModel<String>()
		{
			@Override
			protected String load()
			{
				return String.valueOf(choiceRenderer.getDisplayValue(model.getObject()));
			}
		})
		{
			@Override
			protected void onDetach()
			{
				super.onDetach();
				model.detach();
			}

			{
				add(new AttributeAppender("class", "active")
				{
					{
						setSeparator(" ");
					}

					@Override
					public boolean isEnabled(Component component)
					{
						return Objects.equal(AjaxButtonGroup.this.getDefaultModelObject(), model.getObject());
					};
				});

				{
					add(new Label("label", getModel()));
				}

				addExtraAttributes(this, model.getObject());
			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				AjaxButtonGroup.this.setDefaultModelObject(model.getObject());
				onSelectionChanged(model.getObject(), target, getMarkupId());
				target.appendJavaScript("$('#" + getMarkupId() + "').parent().find('.btn').removeClass('active');$('#" + getMarkupId() + "').addClass('active');");
			}
		};
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(optionsModel);
		ModelUtil.nullSafeDetach(choiceRenderer);
	}

	protected void onSelectionChanged(T selection, AjaxRequestTarget target, String markupId)
	{

	}

	protected void addExtraAttributes(IndicatingAjaxLink button, T selection)
	{

	}

}

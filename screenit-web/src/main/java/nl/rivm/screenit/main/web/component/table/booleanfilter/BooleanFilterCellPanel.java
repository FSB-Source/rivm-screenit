package nl.rivm.screenit.main.web.component.table.booleanfilter;

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

import java.lang.reflect.InvocationTargetException;

import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.topicuszorg.wicket.input.AttributeRemover;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.StringResourceModel;

public abstract class BooleanFilterCellPanel<T> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	private String property;

	public BooleanFilterCellPanel(String id, IModel<T> rowModel, String property, boolean verwijderen, final IDialog dialog, final String confirmResourceKey)
	{
		super(id, rowModel);
		this.property = property;
		setOutputMarkupId(true);
		final WebMarkupContainer toggleActief = new WebMarkupContainer("toggleActief");

		if (verwijderen)
		{
			toggleActief.add(new AjaxEventBehavior("click")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onEvent(AjaxRequestTarget target)
				{
					Boolean actief = BooleanFilterCellPanel.this.getProperty();
					if (!Boolean.FALSE.equals(actief))
					{
						target.appendJavaScript("$('.modal.fade.in').addClass('previousDialog').modal('hide');");
						dialog.setContent(new ConfirmPanel(IDialog.CONTENT_ID, new StringResourceModel(confirmResourceKey, (IModel) null), null, new DefaultConfirmCallback()
						{

							private static final long serialVersionUID = 1L;

							@Override
							public void onYesClick(AjaxRequestTarget target)
							{
								onToggleActief(target);
							}

							@Override
							public void onNoClick(AjaxRequestTarget target)
							{
								super.onNoClick(target);
								target.appendJavaScript("$('.previousDialog').not('#sessieVerlopenDialog').modal('show');");
							}

							@Override
							public void onCloseClick(AjaxRequestTarget target)
							{
								super.onCloseClick(target);
								target.appendJavaScript("$('.previousDialog').not('#sessieVerlopenDialog').modal('show');");
							}

						}, dialog));
						dialog.open(target);
					}
					else
					{
						onToggleActief(target);
					}
				}

				@Override
				protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
				{
					super.updateAjaxAttributes(attributes);
					attributes.getAjaxCallListeners().add(new AjaxCallListener().onPrecondition("return true;"));
				}

				private void onToggleActief(AjaxRequestTarget target)
				{
					setProperty(Boolean.FALSE.equals(getProperty()));
					if (getProperty())
					{
						toggleActief.add(new AttributeRemover("class", Model.of(" niet-actief"), " "));
						toggleActief.add(new AttributeAppender("class", Model.of(" actief")));
					}
					else
					{
						toggleActief.add(new AttributeRemover("class", Model.of(" actief"), " "));
						toggleActief.add(new AttributeAppender("class", Model.of(" niet-actief")));
					}
					BooleanFilterCellPanel.this.onToggleActief(target, getModelObject());
					target.add(toggleActief);
				}

			});
		}
		toggleActief.setOutputMarkupId(true);
		if (!isActief(rowModel))
		{
			toggleActief.add(new AttributeAppender("class", Model.of(" niet-actief")));
		}
		else
		{
			toggleActief.add(new AttributeAppender("class", Model.of(" actief")));
		}
		add(toggleActief);
	}

	private Boolean getProperty()
	{
		try
		{
			return (Boolean) PropertyUtils.getProperty(getModelObject(), property);
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{

		}
		return null;
	}

	private void setProperty(Boolean value)
	{
		try
		{
			PropertyUtils.setProperty(getModelObject(), property, value);
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{

		}
	}

	protected abstract boolean isActief(IModel<T> rowModel);

	protected void onToggleActief(AjaxRequestTarget target, T actiefObject)
	{

	}

}

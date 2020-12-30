package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.model.IActief;
import nl.topicuszorg.wicket.input.AttributeRemover;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class ActiefCellPanel<T extends IActief> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	public ActiefCellPanel(String id, IModel<T> rowModel, boolean verwijderen, final IDialog dialog, final String confirmResourceKey)
	{
		super(id, rowModel);
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
					T actiefObject = ActiefCellPanel.this.getModelObject();
					if (mayToggle(actiefObject))
					{
						Boolean actief = actiefObject.getActief();
						if (!skipConfirmation() && !Boolean.FALSE.equals(actief))
						{
							target.appendJavaScript("$('.modal.fade.in').addClass('previousDialog').modal('hide');");
							dialog.setContent(new ConfirmPanel(IDialog.CONTENT_ID, new SimpleStringResourceModel(confirmResourceKey), null, new DefaultConfirmCallback()
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
				}

				@Override
				protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
				{
					super.updateAjaxAttributes(attributes);
					attributes.getAjaxCallListeners().add(new AjaxCallListener().onPrecondition("return true;"));
				}

				private void onToggleActief(AjaxRequestTarget target)
				{
					T actiefObject = ActiefCellPanel.this.getModelObject();
					actiefObject.setActief(Boolean.FALSE.equals(actiefObject.getActief()));
					if (actiefObject.getActief())
					{
						toggleActief.add(new AttributeRemover("class", Model.of(" niet-actief"), " "));
						toggleActief.add(new AttributeAppender("class", Model.of(" actief")));
					}
					else
					{
						toggleActief.add(new AttributeRemover("class", Model.of(" actief"), " "));
						toggleActief.add(new AttributeAppender("class", Model.of(" niet-actief")));
					}

					target.add(toggleActief);
					ActiefCellPanel.this.onAfterToggleActief(target, actiefObject);
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

	protected boolean skipConfirmation()
	{
		return false;
	}

	protected boolean mayToggle(T actiefObject)
	{
		return true;
	}

	protected abstract boolean isActief(IModel<T> rowModel);

	protected void onAfterToggleActief(AjaxRequestTarget target, T actiefObject)
	{

	}
}

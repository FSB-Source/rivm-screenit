package nl.rivm.screenit.main.web.component;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.service.colon.IFobtService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.behavior.FocusBehavior;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ZoekMetScannedInputPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	private final IModel<String> scanInput = Model.of();

	@SpringBean
	private IFobtService ifobtService;

	public ZoekMetScannedInputPanel(String id)
	{
		super(id);
		add(new ZoekMetScannedInputForm("form"));
	}

	private class ZoekMetScannedInputForm extends Form<Void>
	{
		private static final long serialVersionUID = 1L;

		public ZoekMetScannedInputForm(String id)
		{
			super(id);
			TextField<String> scanTextField = new TextField<>("scanField", scanInput);
			scanTextField.add(new FocusBehavior());
			setOutputMarkupId(true);
			add(scanTextField.add(new AjaxFormComponentUpdatingBehavior("keydown")
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					processScannedInput(target);
				}

				@Override
				protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
				{
					super.updateAjaxAttributes(attributes);
					attributes.getAjaxCallListeners().add(new AjaxCallListener()
					{

						private static final long serialVersionUID = 1L;

						@Override
						public CharSequence getPrecondition(Component component)
						{
							StringBuilder precondition = new StringBuilder();
							precondition.append("var code= (attrs.event.keyCode ? attrs.event.keyCode : attrs.event.which);" + "if (code == 13){ return true; } return false;"
								+ "attrs.event.preventDefault();");
							return precondition.toString();
						}
					});
				}
			}));

			AjaxSubmitLink submitLink = new AjaxSubmitLink("scannen")
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					processScannedInput(target);
				}
			};
			setDefaultButton(submitLink);
			add(submitLink);
		}

	}

	protected void processScannedInput(AjaxRequestTarget target)
	{
	}

	protected String getScanInput()
	{
		return ModelUtil.nullSafeGet(scanInput);
	}

	public void reset(AjaxRequestTarget target)
	{
		scanInput.setObject(null);
		target.add(get("form"));
	}
}

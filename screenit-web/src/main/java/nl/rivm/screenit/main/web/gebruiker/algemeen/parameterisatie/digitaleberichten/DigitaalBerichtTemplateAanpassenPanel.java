package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.digitaleberichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.DigitaalBerichtTemplate;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.enums.DigitaalBerichtType;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.DigitaalBerichtTemplateService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	checkScope = false,
	level = ToegangLevel.LANDELIJK,
	recht = Recht.GEBRUIKER_BEHEER_PARAMETERISATIE,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })

public class DigitaalBerichtTemplateAanpassenPanel extends GenericPanel<DigitaalBerichtTemplate>
{
	private IModel<MergeField> mergeFieldModel;

	private Component laatstGeselecteerdeTextField;

	@SpringBean
	private DigitaalBerichtTemplateService templateService;

	private final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_PARAMETERISATIE, Actie.AANPASSEN);

	public DigitaalBerichtTemplateAanpassenPanel(String id, IModel<DigitaalBerichtTemplate> digitaalBerichtTemplateModel)
	{
		super(id, digitaalBerichtTemplateModel);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		mergeFieldModel = new Model<>();
		var digitaalBerichtTemplate = getModelObject();

		Form<DigitaalBerichtTemplate> form = new ScreenitForm<>("form");
		form.setOutputMarkupId(true);

		maakMergeFieldDropdown(form, digitaalBerichtTemplate.getType());
		maakMergeFieldInvoegenButton(form);
		maakBerichtSubject(form, digitaalBerichtTemplate);
		maakSmsWaarschuwingsTekst(form, digitaalBerichtTemplate);
		maakBerichtBody(form);
		maakFormOpslaanKnop(form);
		add(form);
	}

	private void maakFormOpslaanKnop(Form<DigitaalBerichtTemplate> form)
	{
		var opslaan = new IndicatingAjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				BasePage.markeerFormulierenOpgeslagen(target);
				var template = DigitaalBerichtTemplateAanpassenPanel.this.getModelObject();

				var berichtTemplateNaam = EnumStringUtil.maakStringMetBvoEnEnumPropertyString(template.getType(), this::getString);

				templateService.saveOrUpdateDigitaalBerichtTemplate(template, ScreenitSession.get().getLoggedInAccount(), berichtTemplateNaam);
				info(String.format(getString("opgeslagen"), berichtTemplateNaam));
			}
		};
		opslaan.setVisible(magAanpassen);
		form.add(opslaan);
	}

	private void maakBerichtBody(Form<DigitaalBerichtTemplate> form)
	{
		var berichtBody = new TextArea<>("body");
		berichtBody.add(new AjaxEventBehavior("click")
		{
			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				laatstGeselecteerdeTextField = berichtBody;
			}
		});
		berichtBody.setEnabled(magAanpassen);
		berichtBody.setRequired(true);
		form.add(berichtBody);
	}

	private void maakBerichtSubject(Form<DigitaalBerichtTemplate> form, DigitaalBerichtTemplate digitaalBerichtTemplate)
	{
		var berichtSubject = new TextField<>("subject");
		berichtSubject.add(new AjaxEventBehavior("click")
		{
			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				laatstGeselecteerdeTextField = berichtSubject;
			}
		});
		var isEmailBericht = DigitaalBerichtType.EMAIL == digitaalBerichtTemplate.getType().getBerichtType();
		berichtSubject.setEnabled(magAanpassen);
		berichtSubject.setRequired(true);
		berichtSubject.setVisible(isEmailBericht);
		berichtSubject.add(StringValidator.maximumLength(Constants.MAX_LENGTE_TEMPLATE_SUBJECT));
		form.add(berichtSubject);
	}

	private void maakSmsWaarschuwingsTekst(Form<DigitaalBerichtTemplate> form, DigitaalBerichtTemplate digitaalBerichtTemplate)
	{
		var waarschuwingsTekst = new Label("smsMaxTekensTekst", getString("sms-waarschuwings-tekst"));
		waarschuwingsTekst.setVisible(DigitaalBerichtType.SMS == digitaalBerichtTemplate.getType().getBerichtType());
		form.add(waarschuwingsTekst);
	}

	private void maakMergeFieldDropdown(Form<DigitaalBerichtTemplate> form, DigitaalBerichtTemplateType templateType)
	{
		var gebruikteMergeFields = templateType.getMergeFields();
		var mergeFieldDropdown = new ScreenitDropdown<>("mergefield", mergeFieldModel, gebruikteMergeFields, new ChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(MergeField object)
			{
				return object.getFieldName();
			}
		}).setNullValid(false);
		mergeFieldDropdown.setVisible(magAanpassen);
		form.add(mergeFieldDropdown);

	}

	private void maakMergeFieldInvoegenButton(Form<DigitaalBerichtTemplate> form)
	{
		var mergeFieldInvoegen = new AjaxSubmitLink("invoegen")
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				if (mergeFieldModel.getObject() != null)
				{
					var template = DigitaalBerichtTemplateAanpassenPanel.this.getModelObject();
					var mergeFieldNaam = mergeFieldModel.getObject().getFieldName();
					if (laatstGeselecteerdeTextField != null && "subject".equals(laatstGeselecteerdeTextField.getId()))
					{
						var subjectInhoud = template.getSubject() == null ? "" : template.getSubject();
						subjectInhoud += " {" + mergeFieldNaam + "}";
						template.setSubject(subjectInhoud);
					}
					else
					{
						var bodyInhoud = template.getBody() == null ? "" : template.getBody();
						bodyInhoud += " {" + mergeFieldNaam + "}";
						template.setBody(bodyInhoud);
					}
					target.add(form);
				}
			}
		};
		form.add(mergeFieldInvoegen);
	}
}

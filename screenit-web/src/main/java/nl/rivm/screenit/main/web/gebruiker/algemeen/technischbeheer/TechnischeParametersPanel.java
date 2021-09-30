package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

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

import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.EditOrganisatieParametersPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.ParameterisatiePropertyModel;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;
import org.apache.wicket.validation.validator.RangeValidator;
import org.springframework.scheduling.support.CronSequenceGenerator;

public class TechnischeParametersPanel extends BaseTechnischBeheerParametersPanel
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	public TechnischeParametersPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, new ParameterisatiePropertyModel<>(model));

		boolean magAanpassen = magAanpassen();

		bmhkLabParameters(magAanpassen);
		sosParameters(magAanpassen);
	}

	private void bmhkLabParameters(boolean magAanpassen)
	{
		List<BMHKLaboratorium> actieveLabs = instellingService.getActieveInstellingen(BMHKLaboratorium.class);
		Form<?> labsForm = new Form<>("labsForm");

		ListView<BMHKLaboratorium> labs = new ListView<BMHKLaboratorium>("labs", ModelUtil.listRModel(actieveLabs, false))
		{
			@Override
			protected void populateItem(ListItem<BMHKLaboratorium> item)
			{
				item.setModel(new CompoundPropertyModel<>(item.getModel()));
				item.add(new Label("naam"));
				item.setVisible(!Boolean.FALSE.equals(item.getModelObject().getActief()));
				ComponentHelper.addTextField(item, "orderHost", false, 100, String.class, !magAanpassen);
				ComponentHelper.addTextField(item, "orderPort", false, 5, Integer.class, !magAanpassen);
			}
		};

		labsForm.add(labs);
		add(labsForm);

		AjaxSubmitLink opslaanKnop = new AjaxSubmitLink("parametersOpslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan(target);
				info("Parameters zijn opgeslagen");

			}
		};
		opslaanKnop.setVisible(magAanpassen);
		labsForm.add(opslaanKnop);
	}

	private void sosParameters(boolean magAanpassen)
	{
		add(new EditOrganisatieParametersPanel("soParameters", Collections.singletonList(OrganisatieParameterKey.MAX_MERGED_BRIEVEN_PDF_SIZE_MB), magAanpassen)
		{

			@Override
			protected void addOpslaanButton(Form<Void> form)
			{
				IndicatingAjaxSubmitLink parametersOpslaan = new IndicatingAjaxSubmitLink("parametersOpslaan", form)
				{
					@Override
					protected void onSubmit(AjaxRequestTarget target)
					{
						super.onSubmit(target);
						instellingService.saveOrUpdateOrganisatieParameters(getAllParameters(), ScreenitSession.get().getLoggedInInstellingGebruiker());
						info("Parameters zijn opgeslagen");
					}
				};
				parametersOpslaan.setVisible(magAanpassen);
				TechnischeParametersPanel.this.add(parametersOpslaan);
			}

		});
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");
		form.add(ComponentHelper.newDatePicker("startdatumBmhk", magAanpassen()).setRequired(true));
		form.add(new TextField<>("internalZorgmailBestandUrl", String.class).setRequired(true));
		form.add(new TextField<>("internalWsbSchematronVersionpathmapping", String.class).setRequired(true));
		form.add(new TextField<>("internalMammaSeInformatieOphalenCron", String.class).add(new IValidator<String>()
		{
			@Override
			public void validate(IValidatable<String> validatable)
			{
				if (!CronSequenceGenerator.isValidExpression(validatable.getValue()))
				{
					validatable.error(new ValidationError("Invalide cron expressie"));
				}
			}
		}).setRequired(true));
		form.add(ComponentHelper.newDatePicker("internalColonComplicatieVerwerkingStop", magAanpassen()).setRequired(true));
		int maxKiloBytesZip = 129000;
		form.add(new TextField<>("internalMaxGrootteZip", Integer.class).setRequired(true).add(RangeValidator.range(1, maxKiloBytesZip)));
		form.add(new TextField<>("internalMammaUploadlimietUploadportaal", Integer.class).setRequired(true).add(RangeValidator.minimum(1)));
		form.add(new TextField<>("internalHerinneringsperiodeLogregelOnvolledigAdres", Integer.class).setRequired(true).add(RangeValidator.range(0, 99)));
		form.add(new TextField<>("internalMammaImsDicomCmoveConfig", String.class).setRequired(true));
		form.add(new TextField<>("internalMammaImsDicomCstoreConfig", String.class).setRequired(true));
		form.add(new TextField<>("internalCervixLabFormulierValidFqdns", String.class));
		form.add(new TextField<>("internalUziLoginUrlPrefix", String.class));
		form.add(new CheckBox("bmhkLabelPrintenZonderPdf"));
		return form;
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new AjaxSubmitLink("parametersOpslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan(target);
				info("Parameters zijn opgeslagen");
			}
		};
	}
}

package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.AjaxLazyLoadPanel;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaPostcodeReeksZoekenPage extends MammaPlanningBasePage
{

	private static final long serialVersionUID = 1L;

	private Form<MammaPostcodeReeks> zoekForm;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	public MammaPostcodeReeksZoekenPage()
	{
		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		IModel<MammaPostcodeReeks> criteriaModel;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(MammaPostcodeReeksZoekenPage.class))
		{
			criteriaModel = (IModel<MammaPostcodeReeks>) ScreenitSession.get().getZoekObject(MammaPostcodeReeksZoekenPage.class);
		}
		else
		{
			MammaPostcodeReeks zoekObject = new MammaPostcodeReeks();
			criteriaModel = ModelUtil.cModel(zoekObject);
		}
		setDefaultModel(criteriaModel);

		final WebMarkupContainer refreshContainer = new MammaPostcodeReeksenPanel("resultaten", criteriaModel, true);
		add(refreshContainer);

		AjaxLink<Void> toevoegen = new IndicatingAjaxLink<Void>("postcodeReeksToevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaPostcodeReeks postcodeReeks = new MammaPostcodeReeks();
				setResponsePage(new MammaPostcodeReeksEditPage(ModelUtil.cModel(postcodeReeks)));
			}
		};

		toevoegen.setVisible(ingelogdNamensRegio != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.TOEVOEGEN));
		add(toevoegen);

		zoekForm = new Form<MammaPostcodeReeks>("zoekForm", (IModel<MammaPostcodeReeks>) getDefaultModel());
		add(zoekForm);

		zoekForm.add(new PostcodeField("vanPostcode").setAlleenCijfersToegestaan(true));
		ScreenitDropdown<MammaStandplaats> standplaatsDropdown = new ScreenitDropdown<>("standplaats",
			ModelUtil.listRModel(standplaatsService.getActieveStandplaatsen(ingelogdNamensRegio), false), new ChoiceRenderer<MammaStandplaats>("naam"));

		standplaatsDropdown.setNullValid(true);
		zoekForm.add(standplaatsDropdown);

		ScreenitDropdown<ScreeningOrganisatie> regioDropdown = new ScreenitDropdown<>("standplaats.regio",
			ModelUtil.listRModel(instellingService.getActieveInstellingen(ScreeningOrganisatie.class), false), new ChoiceRenderer<ScreeningOrganisatie>("naam"));

		if (ingelogdNamensRegio == null)
		{
			standplaatsDropdown.setOutputMarkupId(true);
			regioDropdown.setOutputMarkupId(true);
			standplaatsDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					MammaStandplaats standplaats = zoekForm.getModelObject().getStandplaats();
					regioDropdown.setEnabled(standplaats == null);
					target.add(zoekForm);
				}
			});

			regioDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					MammaStandplaats standplaats = zoekForm.getModelObject().getStandplaats();
					ScreeningOrganisatie regio = ingelogdNamensRegio;
					if (standplaats != null && standplaats.getRegio() != null)
					{
						regio = standplaats.getRegio();
					}
					standplaatsDropdown.setChoices(ModelUtil.listRModel(standplaatsService.getActieveStandplaatsen(regio), false));
					target.add(zoekForm);
				}
			});
			regioDropdown.setEnabled(criteriaModel.getObject().getStandplaats() == null || criteriaModel.getObject().getStandplaats().getId() == null);
		}
		else
		{
			regioDropdown.setVisible(false);
		}
		regioDropdown.setNullValid(true);
		zoekForm.add(regioDropdown);

		IndicatingAjaxSubmitLink zoekenBtn = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				ScreenitSession.get().setZoekObject(MammaPostcodeReeksZoekenPage.class, zoekForm.getModel());
				target.add(refreshContainer);
			}
		};

		zoekForm.add(zoekenBtn);

		add(new AjaxLazyLoadPanel("uncoveredPostcodes")
		{

			@Override
			public Component getLazyLoadComponent(String id)
			{
				return new Label(id, StringUtils.join(baseConceptPlanningsApplicatie.getUncoveredPostcodes(ScreenitSession.get().getScreeningOrganisatie()), ", "));
			}

		});

	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.FALSE;
	}

}


package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.component.validator.EmailAddressenValidator;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.Model;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.RangeValidator;

public class ColonPrimaireParametersPanel extends BasePrimaireParametersPanel
{

	private static final long serialVersionUID = 1L;

	public ColonPrimaireParametersPanel(String id, Model<Parameterisatie> model)
	{
		super(id, model);
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");
		form.add(new TextField<>("uitnodigingsinterval", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("vooraankondiginsperiode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("colonVooraankondigingNaVervolgonderzoek", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(createDoubleTextField("percentgageifobtongustig").setRequired(true).add(new RangeValidator<>(0.01, 100.0)));
		form.add(createDoubleTextField("percentageifobtretour").setRequired(true).add(new RangeValidator<>(0.01, 100.0)));
		form.add(new TextField<>("intakeafspraakperiode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("ifobtanalyseperiode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("waarschuwingaantalifobts", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("maximumaantalifobts", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("ifobtrapelperiode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("ifobtretourperiode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(createDoubleTextField("ifobtNormWaarde").setRequired(true));
		form.add(createDoubleTextField("ifobtDetectiegrens").setRequired(true));
		form.add(new TextField<>("minimaleLeeftijdColon", Integer.class).setRequired(true).add(RangeValidator.minimum(0)));
		form.add(new TextField<>("maximaleLeeftijdColon", Integer.class).setRequired(true).add(RangeValidator.minimum(0)));
		form.add(new TextField<>("ongunstigeUitslagWachtPeriode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("colonMaxExtraDagenPlanningIntake", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("maxAfstandClientColoscopiecentrum", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("colonMaxExtraPogingenPlanningIntake", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("colonZonderConclusiePeriode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("colonZonderDefVervolgbeleidPeriode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("afstandfactor", Integer.class).add(RangeValidator.range(0, 100)).setRequired(true));
		form.add(new TextField<>("tijdfactor", Integer.class).add(RangeValidator.range(0, 100)).setRequired(true));
		form.add(new TextField<>("dashboardemail", String.class).add(EmailAddressenValidator.getInstance()));
		form.add(new TextField<>("intakeNietWijzigbaar", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("huisartsNoShowPeriode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("edifactadres", String.class).add(EmailAddressValidator.getInstance()).setRequired(false));
		form.add(new TextField<>("periodeMinimaleHoudbaarheidIfobtMonstersVoorControle", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("wachttijdVerzendenPakketTweeOpEenAdres", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("colonAantalRondesUitnodigingsbriefZonderFit", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));
		form.add(new TextField<>("colonSignaleringstermijnGeenCapaciteit", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));

		addTextAreaField(form, "colonEenmaligHeraanmeldenTekst");
		addTextAreaField(form, "colonTijdelijkHeraanmeldenTekst");
		addTextAreaField(form, "colonDefinitiefHeraanmeldenTekst");
		addTextAreaField(form, "colonDefinitieveAfmeldingBevestigingTekst");
		addTextAreaField(form, "colonTijdelijkeAfmeldingBevestigingTekst");
		addTextAreaField(form, "colonNieuweFitAangevraagdTekst");
		addTextAreaField(form, "colonNieuweFitNaHeraanmeldingTekst");
		addTextAreaField(form, "colonLaatsteRondeBriefTekst");
		addTextAreaField(form, "colonVolgendeRondeBriefTekst");

		return form;
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new IndicatingAjaxSubmitLink("landelijkeParametersOpslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan(target, Bevolkingsonderzoek.COLON);
				info("Landelijke parameters zijn opgeslagen");
			}
		};
	}
}

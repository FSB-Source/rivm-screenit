package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.validation.validator.RangeValidator;

public class CervixPrimaireParametersPanel extends BasePrimaireParametersPanel
{

	private static final long serialVersionUID = 1L;

	public CervixPrimaireParametersPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, model);
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");
		form.add(new TextField<>("uitstelBijZwangerschapCervix", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));

		form.add(new TextField<>("cervixWachttijdUitstrijkjeOntbreektAnaloog", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdWachtOpUitstrijkjeOntvangenAnaloog", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdUitstrijkjeOntbreektDigitaal", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdWachtOpUitstrijkjeOntvangenDigitaal", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdWachtOpHpvUitslag", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdLabformulierOntbreekt", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdHuisartsDoorgeven", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdWachtOpGecontroleerd", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdWachtOpGecontroleerdVoorCytologie", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdWachtOpCytologieUitslag", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixWachttijdWachtOpWaarschuwingCytologieUitslag", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));

		form.add(new TextField<>("periodeMinimaleHoudbaarheidZasMonstersVoorControle", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("afkapwaardeLabformulier", Integer.class).add(RangeValidator.range(0, 1000)).setRequired(true));

		form.add(new TextField<>("periodeUitslagNaarHuisarts", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixHerinneringsPeriodeNonResponder", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixHerinneringsPeriodeZas", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixHerinneringsPeriodeLaatsteHerinnering", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixIntervalControleUitstrijkje", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixMaxZasAanvragenInfolijn", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixMaxZasAanvragenClient", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));

		form.add(new TextField<>("cervixUitstelUitslagbriefPap3a2OfHoger", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("cervixVooraankondigingsPeriode", Integer.class).add(RangeValidator.minimum(0)).add(RangeValidator.maximum(180)).setRequired(true));
		form.add(ComponentHelper.newDatePicker("cervixStartAanleveringGenotyperingEnInvoeringTriage", magAanpassen()).setRequired(true));

		addTextAreaField(form, "cervixHerinneringTekst");
		addTextAreaField(form, "cervixUitgesteldTekst");
		addTextAreaField(form, "cervixEenmaligHeraanmeldenTekst");
		addTextAreaField(form, "cervixDefinitiefHeraanmeldenTekst");

		addTextAreaField(form, "cervixVervolgonderzoekNegatief65plusTekst");
		addTextAreaField(form, "cervixVervolgonderzoekNegatief60plusTekst");
		addTextAreaField(form, "cervixVervolgonderzoekNegatiefOverigeTekst");

		addTextAreaField(form, "cervixCytologiePositief60plusTekst");
		addTextAreaField(form, "cervixCytologiePositiefOverigeTekst");
		return form;
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new AjaxSubmitLink("landelijkeParametersOpslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan(target, Bevolkingsonderzoek.CERVIX);
				info("Landelijke parameters zijn opgeslagen");

			}
		};
	}
}

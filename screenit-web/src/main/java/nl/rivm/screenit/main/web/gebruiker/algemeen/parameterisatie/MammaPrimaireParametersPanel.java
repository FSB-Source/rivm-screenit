package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.Model;
import org.apache.wicket.validation.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.RangeValidator;

public class MammaPrimaireParametersPanel extends BasePrimaireParametersPanel
{

	private static final long serialVersionUID = 1L;

	public MammaPrimaireParametersPanel(String id, Model<Parameterisatie> model)
	{
		super(id, model);
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");

		form.add(new TextField<>("mammaMinimaleLeeftijd", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaMaximaleLeeftijd", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaMinimaleIntervalMammografieOnderzoeken", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaMinimaleIntervalUitnodigingen", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaAfspraakBijUitnodigenVanafAantalWerkdagen", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaAfspraakVerzettenZonderClientContactVanafAantalWerkdagen", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaCapaciteitVolledigBenutTotEnMetAantalWerkdagen", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaBulkVerzettenInVerledenAantalWeken", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaFollowUpNietGedownloadWerklijstNaDagen", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaFollowUpRadiologieWerklijstNaDownloaden", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaFollowUpPathologieWerklijstNaRadiologieverslag", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaHerinneringsPeriodeGeenAfspraak", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaHerinneringsPeriodeNoShow", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaSeMaxOfflineInlogperiode", Integer.class).add(RangeValidator.minimum(0)).setRequired(true));
		form.add(new TextField<>("mammaSeDaglijstOphalenDagen", Integer.class).add(RangeValidator.minimum(0)).add(RangeValidator.maximum(14)).setRequired(true));
		addTextAreaField(form, "mammaOnderbrokenOnderzoekZonderBeeldenTekst");
		addTextAreaField(form, "mammaOnderbrokenOnderzoekMetBeeldenTekst");
		addTextAreaField(form, "mammaBulkVerzettenVerledenAfspraakTekst");
		addTextAreaField(form, "mammaBulkVerzettenToekomstAfspraakTekst");
		addTextAreaField(form, "mammaAfspraakLocatieWijzigingTekst");
		form.add(new TextField<>("mammaMeekijkverzoekMailAdres", String.class).add(EmailAddressValidator.getInstance()).setRequired(true));
		form.add(new CheckBox("mammaPalgaExportAlleenVerwezen"));
		return form;
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new IndicatingAjaxSubmitLink("landelijkeParametersOpslaan")
		{

			private static final long serialVersionUID = 1L;

			protected void onSubmit(AjaxRequestTarget target)
			{

				Parameterisatie parameterisatie = getModelObject();
				Map<PreferenceKey, Object> oudeParameters = getOudParameterObject().getParameters();

				if ((Integer) parameterisatie.getParameters().get(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD) > (Integer) parameterisatie.getParameters()
					.get(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD))
				{
					error(getString("error.minimumleeftijd.na.maximum"));
				}
				else
				{
					boolean leeftijdParametersZijnAangepast = !oudeParameters.get(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD)
						.equals(parameterisatie.getParameters().get(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD)) ||
						!oudeParameters.get(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD)
							.equals(parameterisatie.getParameters().get(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD));

					opslaan(target, Bevolkingsonderzoek.MAMMA);

					if (leeftijdParametersZijnAangepast)
					{
						warn(getString("warn.leeftijdparameters.zijn.aangepast"));
					}
					else
					{
						info(getString("info.parameters.opgeslagen"));
					}
				}
			}
		};
	}

}

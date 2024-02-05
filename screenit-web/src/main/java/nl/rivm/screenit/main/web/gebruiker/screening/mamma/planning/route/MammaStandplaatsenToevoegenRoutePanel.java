package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaRouteService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaStandplaatsenToevoegenRoutePanel extends GenericPanel<MammaScreeningsEenheid>
{

	@SpringBean
	private MammaRouteService routeService;

	private IModel<List<MammaStandplaats>> standplaatsenModel = ModelUtil.listModel(new ArrayList<>());

	public MammaStandplaatsenToevoegenRoutePanel(String id, IModel<MammaScreeningsEenheid> model)
	{
		super(id, model);

		Form form = new Form("form");
		add(form);

		form.add(new IndicatingAjaxSubmitLink("toevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				List<MammaStandplaats> toegevoegdeStandplaatsList = standplaatsenModel.getObject();

				toegevoegdeStandplaatsList.stream()
					.filter(toegevoegdeStandplaats -> toegevoegdeStandplaats.getPostcodeReeksen().isEmpty() && toegevoegdeStandplaats.getTehuizen().isEmpty())
					.forEach(standplaatsZonderPostcodeReeksenOfTehuizen -> {
						error("Standplaats '" + standplaatsZonderPostcodeReeksenOfTehuizen.getNaam() + "' heeft geen gekoppelde postcodereeksen of tehuizen.");
					});
				toegevoegdeStandplaatsList.stream().filter(toegevoegdeStandplaats -> heeftOnvolledigeDatumOfLocatieGegevens(toegevoegdeStandplaats))
					.forEach(standplaatsZonderValideLocatieGegevens -> {
						error("Standplaats '" + standplaatsZonderValideLocatieGegevens.getNaam() + "' heeft onvolledige locatiegegevens.");
					});

				if (!hasErrorMessage())
				{
					routeService.standplaatsenToevoegenRoute(MammaStandplaatsenToevoegenRoutePanel.this.getModelObject(), standplaatsenModel.getObject(),
						ScreenitSession.get().getLoggedInInstellingGebruiker());
					standplaatsenToegevoegd(target);
				}
			}

			private boolean heeftOnvolledigeDatumOfLocatieGegevens(MammaStandplaats standplaats)
			{
				boolean heeftOnvolledigeDatumOfLocatieGegegevens = heeftOnvolledigeLocatieGegegevens(standplaats.getLocatie());
				if (standplaats.getTijdelijkeLocatie().getStartDatum() != null || standplaats.getTijdelijkeLocatie().getEindDatum() != null)
				{
					heeftOnvolledigeDatumOfLocatieGegegevens |= heeftOnvolledigeLocatieGegegevens(standplaats.getTijdelijkeLocatie());
				}

				return heeftOnvolledigeDatumOfLocatieGegegevens;
			}

			private boolean heeftOnvolledigeLocatieGegegevens(MammaStandplaatsLocatie locatie)
			{
				return StringUtils.isBlank(locatie.getPostcode()) || StringUtils.isBlank(locatie.getPlaats()) || StringUtils.isBlank(locatie.getStraat())
					|| locatie.getToonHuisnummerInBrieven() && locatie.getHuisnummer() == null;
			}
		});

		form.add(new AjaxLink<Void>("close")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});

		List<MammaStandplaats> standplaatsenZonderRoute = routeService.getStandplaatsenZonderRoute(ScreenitSession.get().getScreeningOrganisatie());
		ScreenitListMultipleChoice<MammaStandplaats> standplaatsen = new ScreenitListMultipleChoice<>("standplaatsen", standplaatsenModel,
			ModelUtil.listRModel(standplaatsenZonderRoute),
			new ChoiceRenderer<>("naam"));
		standplaatsen.setRequired(true);
		form.add(standplaatsen);
	}

	protected abstract void standplaatsenToegevoegd(AjaxRequestTarget target);

	protected abstract void close(AjaxRequestTarget target);
}

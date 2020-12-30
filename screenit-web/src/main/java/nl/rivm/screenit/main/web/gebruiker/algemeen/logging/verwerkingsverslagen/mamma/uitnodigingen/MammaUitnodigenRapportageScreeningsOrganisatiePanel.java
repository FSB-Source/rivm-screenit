package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.mamma.uitnodigingen;

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

import java.io.Serializable;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsPeriodeUitnodigenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeRapportageStatus;
import nl.topicuszorg.wicket.model.SortingListModel;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public class MammaUitnodigenRapportageScreeningsOrganisatiePanel extends GenericPanel<List<MammaStandplaatsPeriodeUitnodigenRapportage>>
{

	public MammaUitnodigenRapportageScreeningsOrganisatiePanel(String id, IModel<List<MammaStandplaatsPeriodeUitnodigenRapportage>> model,
		ScreeningOrganisatie screeningOrganisatie)
	{
		super(id, model);

		add(new Label("naam", screeningOrganisatie.getNaam()));

		SortingListModel<MammaStandplaatsPeriodeUitnodigenRapportage> sortingListModel = new SortingListModel<>(model,
			Comparator
				.comparing((Function<MammaStandplaatsPeriodeUitnodigenRapportage, String> & Serializable) (
					MammaStandplaatsPeriodeUitnodigenRapportage standplaatsPeriodeUitnodigenRapportage) -> standplaatsPeriodeUitnodigenRapportage.getStandplaatsPeriode()
						.getScreeningsEenheid().getNaam())
				.thenComparing((Function<MammaStandplaatsPeriodeUitnodigenRapportage, String> & Serializable) (
					MammaStandplaatsPeriodeUitnodigenRapportage standplaatsPeriodeUitnodigenRapportage) -> standplaatsPeriodeUitnodigenRapportage
						.getStandplaatsRondeUitnodigenRapportage().getStandplaatsRonde().getStandplaats().getNaam()));

		add(new PropertyListView<MammaStandplaatsPeriodeUitnodigenRapportage>("standplaatsPeriodeUitnodigenRapportages", sortingListModel)
		{
			@Override
			protected void populateItem(ListItem<MammaStandplaatsPeriodeUitnodigenRapportage> listItem)
			{
				listItem.setModel(new CompoundPropertyModel<>(listItem.getModel()));
				boolean uitnodigenFout = MammaStandplaatsRondeRapportageStatus.FOUT.equals(listItem.getModelObject().getStandplaatsRondeUitnodigenRapportage().getStatus());
				listItem.add(new Label("standplaatsPeriode.screeningsEenheid.naam"));
				listItem.add(new Label("uitnodigenTotEnMet"));
				listItem.add(new Label("standplaatsPeriode.standplaatsRonde.standplaats.naam"));
				listItem.add(new Label("uitgenodigdAfspraak", uitnodigenFout ? "" : listItem.getModel().getObject().getUitgenodigdAfspraak()));
				listItem.add(new Label("uitgenodigdOpen", uitnodigenFout ? "" : listItem.getModel().getObject().getUitgenodigdOpen()));
				listItem.add(new Label("uitgenodigdMinderValide", uitnodigenFout ? "" : listItem.getModel().getObject().getUitgenodigdMinderValide()));
				listItem.add(new Label("uitgenodigdSuspect", uitnodigenFout ? "" : listItem.getModel().getObject().getUitgenodigdSuspect()));
				listItem.add(new Label("uitgenodigdNaUitstel", uitnodigenFout ? "" : listItem.getModel().getObject().getUitgenodigdNaUitstel()));
				listItem.add(new Label("uitgesteldAchtervangUitstel", uitnodigenFout ? "" : listItem.getModel().getObject().getUitgesteldAchtervangUitstel()));
				listItem.add(new Label("uitgesteldMinderValideUitgewijktUitstel", uitnodigenFout ? "" : listItem.getModel().getObject().getUitgesteldMinderValideUitgewijktUitstel()));
			}
		});
	}
}

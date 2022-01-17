package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.gevolgenlabprocesverwerken;

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

import nl.rivm.screenit.main.web.component.BriefTypeLabel;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixGevolgenLabprocesVerwerkenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixGevolgenLabprocesVerwerkenRapportageBriefType;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public class GevolgenLabprocesVerwerkenVerslagPanel extends GenericPanel<CervixGevolgenLabprocesVerwerkenRapportage>
{

	private static final long serialVersionUID = 1L;

	public GevolgenLabprocesVerwerkenVerslagPanel(String id, IModel<CervixGevolgenLabprocesVerwerkenRapportage> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("datumVerwerking", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("aantalInLabproces"));
		add(new Label("aantalDefinitiefHeraangemeld"));
		add(new Label("aantalEenmaligHeraangemeld"));
		add(new Label("aantalUitnodigingenUitstrijkje"));
		add(new Label("aantalUitnodigingenZas"));
		add(new Label("aantalInVervolgonderzoek"));
		add(new Label("aantalRondenGesloten"));
		add(new Label("totaalAantalBrieven"));
		add(new Label("totaalAantalHuisartsberichten"));

		add(new PropertyListView<CervixGevolgenLabprocesVerwerkenRapportageBriefType>("briefTypen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<CervixGevolgenLabprocesVerwerkenRapportageBriefType> entry)
			{
				entry.add(new BriefTypeLabel("briefType"));
				entry.add(new Label("aantal"));
			}
		});

		add(new PropertyListView<CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType>("huisartsberichtTypen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType> entry)
			{
				entry.add(new Label("huisartsBerichtType", entry.getModelObject().getHuisartsBerichtType().getNaam()));
				entry.add(new Label("aantal"));
			}
		});
	}
}

package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.mamma.uitnodigingen;

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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsPeriodeUitnodigenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeUitnodigenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaUitnodigenRapportage;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaUitnodigenRapportagePanel extends GenericPanel<MammaUitnodigenRapportage>
{
	public MammaUitnodigenRapportagePanel(String id, IModel<MammaUitnodigenRapportage> model)
	{
		super(id, model);
		MammaUitnodigenRapportage uitnodigenRapportage = model.getObject();

		Map<ScreeningOrganisatie, List<MammaStandplaatsPeriodeUitnodigenRapportage>> map = new HashMap<>();
		for (MammaStandplaatsRondeUitnodigenRapportage standplaatsRondeUitnodigenRapportage : uitnodigenRapportage.getStandplaatsRondeUitnodigenRapportages())
		{
			ScreeningOrganisatie screeningOrganisatie = standplaatsRondeUitnodigenRapportage.getStandplaatsRonde().getStandplaats().getRegio();
			List<MammaStandplaatsPeriodeUitnodigenRapportage> standplaatsPeriodeUitnodigenRapportages = map.get(screeningOrganisatie);
			if (standplaatsPeriodeUitnodigenRapportages == null)
			{
				standplaatsPeriodeUitnodigenRapportages = new ArrayList<>();
				map.put(screeningOrganisatie, standplaatsPeriodeUitnodigenRapportages);
			}
			standplaatsPeriodeUitnodigenRapportages.addAll(standplaatsRondeUitnodigenRapportage.getStandplaatsPeriodeUitnodigenRapportages());
		}

		add(DateLabel.forDatePattern("datumVerwerking", new Model<>(uitnodigenRapportage.getDatumVerwerking()), "dd-MM-yyyy HH:mm:ss"));

		RepeatingView repeatingView = new RepeatingView("soTables");
		ToegangLevel toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_MAMMA_UITNODIGEN_VERWERKING_VERSLAG);

		if (toegangLevel == ToegangLevel.LANDELIJK)
		{
			map.forEach(((screeningOrganisatie, standplaatsPeriodeUitnodigenRapportages) -> repeatingView.add(
				new MammaUitnodigenRapportageScreeningsOrganisatiePanel(repeatingView.newChildId(), ModelUtil.listRModel(standplaatsPeriodeUitnodigenRapportages),
					screeningOrganisatie))));
		}
		else
		{
			ScreeningOrganisatie screeningOrganisatie = ScreenitSession.get().getScreeningOrganisatie();
			List<MammaStandplaatsPeriodeUitnodigenRapportage> standplaatsPeriodeUitnodigenRapportages = map.get(screeningOrganisatie);
			if (standplaatsPeriodeUitnodigenRapportages != null)
			{
				repeatingView.add(new MammaUitnodigenRapportageScreeningsOrganisatiePanel(repeatingView.newChildId(), ModelUtil.listRModel(standplaatsPeriodeUitnodigenRapportages),
					screeningOrganisatie));
			}
		}

		add(repeatingView);
	}
}

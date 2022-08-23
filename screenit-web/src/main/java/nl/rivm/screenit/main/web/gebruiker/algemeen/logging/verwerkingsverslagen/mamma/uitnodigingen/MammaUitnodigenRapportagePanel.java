package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.mamma.uitnodigingen;

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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIntervalUitnodigenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsPeriodeUitnodigenRapportage;
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

		var rapportagesPerRegio = splitsRapportagePerRegio(uitnodigenRapportage);

		add(DateLabel.forDatePattern("datumVerwerking", new Model<>(uitnodigenRapportage.getDatumVerwerking()), "dd-MM-yyyy HH:mm:ss"));

		RepeatingView repeatingView = new RepeatingView("soTables");
		ToegangLevel toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_MAMMA_UITNODIGEN_VERWERKING_VERSLAG);

		if (toegangLevel == ToegangLevel.LANDELIJK)
		{
			rapportagesPerRegio.forEach(((screeningOrganisatie, regioRapportage) -> addRegioRapportagePanel(repeatingView, screeningOrganisatie, regioRapportage)));
		}
		else
		{
			ScreeningOrganisatie screeningOrganisatie = ScreenitSession.get().getScreeningOrganisatie();
			var regioRapportage = rapportagesPerRegio.get(screeningOrganisatie);
			if (regioRapportage != null)
			{
				addRegioRapportagePanel(repeatingView, screeningOrganisatie, regioRapportage);
			}
		}

		add(repeatingView);
	}

	private Map<ScreeningOrganisatie, RegioRapportage> splitsRapportagePerRegio(MammaUitnodigenRapportage uitnodigenRapportage)
	{
		Map<ScreeningOrganisatie, RegioRapportage> rapportagesPerRegio = new HashMap<>();
		splitsStandplaatsRondeRapportages(uitnodigenRapportage, rapportagesPerRegio);
		splitsIntervalRapportages(uitnodigenRapportage, rapportagesPerRegio);
		return rapportagesPerRegio;
	}

	private void splitsStandplaatsRondeRapportages(MammaUitnodigenRapportage uitnodigenRapportage, Map<ScreeningOrganisatie, RegioRapportage> rapportagesPerRegio)
	{
		for (var standplaatsRondeRapportage : uitnodigenRapportage.getStandplaatsRondeUitnodigenRapportages())
		{
			var screeningOrganisatie = standplaatsRondeRapportage.getStandplaatsRonde().getStandplaats().getRegio();
			var regioRapportage = getOrCreateRapportageVoorRegio(rapportagesPerRegio, screeningOrganisatie);
			regioRapportage.standplaatsPeriodeRapportages.addAll(standplaatsRondeRapportage.getStandplaatsPeriodeUitnodigenRapportages());
		}
	}

	private RegioRapportage getOrCreateRapportageVoorRegio(Map<ScreeningOrganisatie, RegioRapportage> rapportagesPerRegio, ScreeningOrganisatie screeningOrganisatie)
	{
		return rapportagesPerRegio.computeIfAbsent(screeningOrganisatie, k -> new RegioRapportage());
	}

	private void splitsIntervalRapportages(MammaUitnodigenRapportage uitnodigenRapportage, Map<ScreeningOrganisatie, RegioRapportage> rapportagesPerRegio)
	{
		for (MammaIntervalUitnodigenRapportage intervalRapportage : uitnodigenRapportage.getIntervalUitnodigenRapportages())
		{
			var regioRapportage = getOrCreateRapportageVoorRegio(rapportagesPerRegio, intervalRapportage.getScreeningOrganisatie());
			regioRapportage.intervalRapportage = intervalRapportage;
		}
	}

	private void addRegioRapportagePanel(RepeatingView repeatingView, ScreeningOrganisatie screeningOrganisatie, RegioRapportage regioRapportage)
	{
		repeatingView.add(new MammaUitnodigenRapportageScreeningsOrganisatiePanel(repeatingView.newChildId(),
			ModelUtil.listRModel(regioRapportage.standplaatsPeriodeRapportages), screeningOrganisatie, ModelUtil.csModel(regioRapportage.intervalRapportage)));
	}

	private static class RegioRapportage
	{
		List<MammaStandplaatsPeriodeUitnodigenRapportage> standplaatsPeriodeRapportages = new ArrayList<>();

		MammaIntervalUitnodigenRapportage intervalRapportage;
	}

}

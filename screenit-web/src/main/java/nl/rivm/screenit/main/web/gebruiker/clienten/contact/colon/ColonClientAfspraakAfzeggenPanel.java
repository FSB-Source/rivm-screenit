package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.util.DateUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ColonClientAfspraakAfzeggenPanel extends GenericPanel<ColonIntakeAfspraak>
{
	private final IModel<Boolean> briefTegenhouden = Model.of(Boolean.FALSE);

	public ColonClientAfspraakAfzeggenPanel(String id, IModel<ColonIntakeAfspraak> afspraakModel)
	{
		super(id, afspraakModel);
		add(new Label("vanaf", DateUtil.LOCAL_DATE_TIME_FORMAT.format(afspraakModel.getObject().getVanaf())));

		add(new Label("kamer.intakelocatie.naam"));
		briefTegenhoudenContainer();
	}

	private void briefTegenhoudenContainer()
	{
		var rechtVoorBriefTegenhouden = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN);
		var briefTegenhoudenCheckBox = ComponentHelper.newCheckBox("briefTegenhouden", briefTegenhouden);
		briefTegenhoudenCheckBox.setVisible(rechtVoorBriefTegenhouden);
		briefTegenhoudenCheckBox.setOutputMarkupPlaceholderTag(true);
		add(briefTegenhoudenCheckBox);
	}

	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();
		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK, getModelObject());
		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_STATUS, ColonAfspraakStatus.GEANNULEERD_VIA_INFOLIJN);
		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN, briefTegenhouden.getObject());
		return opslaanObjecten;

	}

	public List<String> getOpslaanMeldingen()
	{
		ColonIntakeAfspraak afspraak = getModelObject();
		ColonScreeningRonde ronde = afspraak.getColonScreeningRonde();
		String melding = String.format("coloscopie intake afspraak van %1$s in %2$s van %3$s afzeggen.", DateUtil.formatShortDateTime(afspraak.getVanaf()),
			afspraak.getKamer().getNaam(),
			afspraak.getKamer().getIntakelocatie().getNaam());

		if (ronde.getOpenUitnodiging() == null)
		{
			melding += " Hiermee wordt de lopende ronde afgerond.";
		}
		return List.of(melding);
	}
}

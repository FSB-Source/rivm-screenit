package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;

import org.apache.wicket.model.IModel;

public class MammaClientContactRondeForcerenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private MammaAfspraakKiezenPanel afspraakKiezenPanel;

	private ClientContactPanel.ClientContactPanelCreateContext clientContactPanelCreateContext;

	public MammaClientContactRondeForcerenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		afspraakKiezenPanel = new MammaAfspraakKiezenPanel("afspraakKiezenPanel", client, true);
		add(afspraakKiezenPanel);
		clientContactPanelCreateContext = new ClientContactPanel.ClientContactPanelCreateContext();
		clientContactPanelCreateContext.bkVanuitPlanning = extraPanelParams.stream().anyMatch(p -> Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING.equals(p.toString()));

	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		if (getPage().getMetaData(ClientContactPanel.CREATE_CONTEXT_KEY) == null)
		{
			getPage().setMetaData(ClientContactPanel.CREATE_CONTEXT_KEY, clientContactPanelCreateContext);
		}
	}

	@Override
	public void validate()
	{
		afspraakKiezenPanel.validate();
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> opslaanMeldingen = super.getOpslaanMeldingen();
		MammaAfspraak afspraak = afspraakKiezenPanel.getNieuweAfspraak();
		SimpleDateFormat dateFormat = new SimpleDateFormat("EEEE dd-MM-yyyy HH:mm");
		opslaanMeldingen.add(String.format("De afspraak wordt gemaakt op %s in %s met %s", dateFormat.format(afspraak.getVanaf()),
			afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getNaam(),
			afspraak.getCapaciteitBlok().getScreeningsEenheid().getNaam()));
		return opslaanMeldingen;

	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		opslaanObjecten.putAll(afspraakKiezenPanel.getOpslaanObjecten());
		return opslaanObjecten;

	}
}

package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel.ClientContactPanelCreateContext;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.mamma.MammaAfspraak;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaClientContactAfspraakMakenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	private Panel afspraakWijzigenPanel;

	private ClientContactPanelCreateContext clientContactPanelCreateContext;

	@SpringBean
	private MammaAfspraakService afspraakService;

	public MammaClientContactAfspraakMakenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		add(new MammaNieuwAfspraakStartPanel("afspraakPanel", client)
		{
			@Override
			public void afspraakAanmaken(AjaxRequestTarget target, IModel<Client> clientModel)
			{
				MammaAfspraakKiezenPanel newAfspraakWijzigenPanel = new MammaAfspraakKiezenPanel("afspraakWijzigenPanel", clientModel);
				newAfspraakWijzigenPanel.setOutputMarkupId(true);

				afspraakWijzigenPanel.replaceWith(newAfspraakWijzigenPanel);
				afspraakWijzigenPanel = newAfspraakWijzigenPanel;
				target.add(afspraakWijzigenPanel);
			}

			@Override
			public void uitstellen(AjaxRequestTarget target, IModel<Client> clientModel)
			{
				MammaUitstelKiezenPanel newAfspraakWijzigenPanel = new MammaUitstelKiezenPanel("afspraakWijzigenPanel", clientModel);
				newAfspraakWijzigenPanel.setOutputMarkupId(true);

				afspraakWijzigenPanel.replaceWith(newAfspraakWijzigenPanel);
				afspraakWijzigenPanel = newAfspraakWijzigenPanel;
				target.add(afspraakWijzigenPanel);
			}

			@Override
			public boolean magUitstellen()
			{
				ClientContactActieType contactActieType = MammaClientContactAfspraakMakenPanel.this.getModelObject().getType();
				return afspraakService.magUitstellen(client.getObject().getMammaDossier(), ClientContactActieType.MAMMA_AFSPRAAK_MAKEN_FORCEREN.equals(contactActieType));
			}

			@Override
			protected boolean rondeForcerenMeldingTonen()
			{
				return extraPanelParams.stream().anyMatch(p -> Constants.RONDE_FORCEREN_MELDING_BIJ_AFSPRAAK_MAKEN.equals(p.toString()));
			}
		});

		afspraakWijzigenPanel = new EmptyPanel("afspraakWijzigenPanel");

		afspraakWijzigenPanel.setOutputMarkupId(true);
		add(afspraakWijzigenPanel);

		clientContactPanelCreateContext = new ClientContactPanel.ClientContactPanelCreateContext();
		clientContactPanelCreateContext.bkVanuitPlanning = extraPanelParams.stream().anyMatch(p -> Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING.equals(p.toString()));
		clientContactPanelCreateContext.bkAlleenClientContact = extraPanelParams.stream()
			.anyMatch(p -> Constants.CONTACT_EXTRA_PARAMETER_ALLEEN_CLIENT_CONTACT.equals(p.toString()));
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
		if (afspraakWijzigenPanel instanceof AbstractClientContactActiePanel)
		{
			((AbstractClientContactActiePanel) afspraakWijzigenPanel).validate();
		}
		else
		{
			error(getString("error.clientafspraakwijzigen.geenwijziging"));
		}
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> opslaanMeldingen = super.getOpslaanMeldingen();
		if (afspraakWijzigenPanel instanceof MammaAfspraakKiezenPanel)
		{
			MammaAfspraak afspraak = ((MammaAfspraakKiezenPanel) afspraakWijzigenPanel).getNieuweAfspraak();
			SimpleDateFormat dateFormat = new SimpleDateFormat("EEEE dd-MM-yyyy HH:mm");
			opslaanMeldingen.add(String.format("De afspraak wordt gemaakt op %s in %s met %s", dateFormat.format(afspraak.getVanaf()),
				afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getNaam(),
				afspraak.getCapaciteitBlok().getScreeningsEenheid().getNaam()));
		}
		else if (afspraakWijzigenPanel instanceof AbstractClientContactActiePanel)
		{
			opslaanMeldingen.addAll(((AbstractClientContactActiePanel) afspraakWijzigenPanel).getOpslaanMeldingen());
		}
		return opslaanMeldingen;

	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		if (afspraakWijzigenPanel instanceof AbstractClientContactActiePanel)
		{
			opslaanObjecten.putAll(((AbstractClientContactActiePanel) afspraakWijzigenPanel).getOpslaanObjecten());
		}
		return opslaanObjecten;

	}
}

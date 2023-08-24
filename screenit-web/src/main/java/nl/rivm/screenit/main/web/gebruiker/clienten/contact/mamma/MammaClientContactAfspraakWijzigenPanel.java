package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.main.web.gebruiker.clienten.agenda.MammaAfspraakPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel.ClientContactPanelCreateContext;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public class MammaClientContactAfspraakWijzigenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	private Panel afspraakWijzigenPanel;

	private ClientContactPanelCreateContext clientContactPanelCreateContext;

	public MammaClientContactAfspraakWijzigenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		add(new MammaAfspraakPanel("afspraakPanel", client)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void verzetten(AjaxRequestTarget target, MammaAfspraak afspraak)
			{
				afspraak = (MammaAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				MammaClientAfspraakVerzettenPanel newAfspraakWijzigenPanel = new MammaClientAfspraakVerzettenPanel("afspraakWijzigenPanel", afspraak);
				newAfspraakWijzigenPanel.setOutputMarkupId(true);

				afspraakWijzigenPanel.replaceWith(newAfspraakWijzigenPanel);
				afspraakWijzigenPanel = newAfspraakWijzigenPanel;
				target.add(afspraakWijzigenPanel);
			}

			@Override
			public void uitstellen(AjaxRequestTarget target, MammaAfspraak afspraak)
			{
				afspraak = (MammaAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				MammaClientAfspraakUitstellenPanel newAfspraakWijzigenPanel = new MammaClientAfspraakUitstellenPanel("afspraakWijzigenPanel", afspraak);
				newAfspraakWijzigenPanel.setOutputMarkupId(true);

				afspraakWijzigenPanel.replaceWith(newAfspraakWijzigenPanel);
				afspraakWijzigenPanel = newAfspraakWijzigenPanel;
				target.add(afspraakWijzigenPanel);
			}

			@Override
			public void afmelden(AjaxRequestTarget target, MammaAfspraak afspraak)
			{
				afspraak = (MammaAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				target.appendJavaScript(
					"$(\"label:contains('" + getString(EnumStringUtil.getPropertyString(ClientContactActieType.MAMMA_AFMELDEN)) + "') input:checkbox\").trigger('click')");
			}
		});

		clientContactPanelCreateContext = new ClientContactPanel.ClientContactPanelCreateContext();
		clientContactPanelCreateContext.bkVanuitPlanning = extraPanelParams.stream().anyMatch(p -> Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING.equals(p.toString()));
		if (extraPanelParams.size() > 1)
		{
			MammaAfspraak afspraak = (MammaAfspraak) extraPanelParams.stream().filter(p -> p instanceof MammaAfspraak).findFirst().orElse(null);
			MammaAfspraakStatus afspraakStatus = (MammaAfspraakStatus) extraPanelParams.stream().filter(p -> p instanceof MammaAfspraakStatus).findFirst().orElse(null);

			switch (afspraakStatus)
			{
			case GEPLAND:
				afspraakWijzigenPanel = new MammaClientAfspraakVerzettenPanel("afspraakWijzigenPanel", afspraak);
				break;
			case UITGESTELD:
				afspraakWijzigenPanel = new MammaClientAfspraakUitstellenPanel("afspraakWijzigenPanel", afspraak);
				break;
			default:
				throw new IllegalStateException();
			}
		}
		else
		{
			afspraakWijzigenPanel = new EmptyPanel("afspraakWijzigenPanel");
		}
		afspraakWijzigenPanel.setOutputMarkupId(true);
		add(afspraakWijzigenPanel);
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
		if (afspraakWijzigenPanel instanceof AbstractClientContactActiePanel)
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

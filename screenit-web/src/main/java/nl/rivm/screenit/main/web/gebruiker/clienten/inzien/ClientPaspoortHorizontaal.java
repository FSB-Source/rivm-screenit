package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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

import nl.rivm.screenit.main.service.algemeen.DeelnamemodusService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.PostcodeLabel;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.MammaDoelgroepIndicatorPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.PatternDateConverter;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientPaspoortHorizontaal extends GenericPanel<Client>
{
	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private DeelnamemodusService deelnamemodusService;

	private final boolean metTelefoonnummer;

	public ClientPaspoortHorizontaal(String id, IModel<Client> clientModel, boolean metTelefoonnummer)
	{
		super(id, clientModel);
		this.metTelefoonnummer = metTelefoonnummer;
	}

	public ClientPaspoortHorizontaal(String id, IModel<Client> clientModel)
	{
		this(id, clientModel, false);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		vulPersoonsGegevens();
	}

	private void vulPersoonsGegevens()
	{
		var client = getModelObject();
		var persoon = client.getPersoon();

		add(new Label("volledigeNaam", NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client)));
		add(new MammaDoelgroepIndicatorPanel("doelgroep", client.getMammaDossier(), true));

		add(new Label("persoon.bsn"));
		add(new Label("persoon.anummer").setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_INZIEN_A_NUMMER, Actie.INZIEN)));
		add(new EnumLabel<>("persoon.geslacht").setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_TOON_GENDERINDETITEIT, Actie.INZIEN)));
		add(new Label("persoon.achternaam", NaamUtil.getGeboorteTussenvoegselEnAchternaam(persoon)));

		add(new Label("persoon.telefoonnummer1").setVisible(metTelefoonnummer));
		add(new Label("persoon.telefoonnummer2").setVisible(metTelefoonnummer));

		add(new DateLabel("persoon.overlijdensdatum", new PatternDateConverter("dd-MM-yyyy", true))
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ClientPaspoortHorizontaal.this.getModelObject().getPersoon().getOverlijdensdatum() != null);
			}
		});

		add(new Label("persoon.geboortedatum", DateUtil.getGeboortedatum(persoon) + " (" + clientService.getLeeftijd(client) + " jaar)"));

		add(new Label("gbaLocatiebeschrijving", (IModel<Object>) () ->
		{
			String locatiebeschrijving = "";
			if (getModelObject().getPersoon().getGbaAdres() != null)
			{
				locatiebeschrijving = AdresUtil.getAdres(getModelObject().getPersoon().getGbaAdres());
			}
			return locatiebeschrijving;
		}));
		add(new PostcodeLabel("gbaPostcode", true,
			(IModel<Object>) () -> getModelObject().getPersoon().getGbaAdres() != null && StringUtils.isNotBlank(getModelObject().getPersoon().getGbaAdres().getPostcode())
				? getModelObject().getPersoon().getGbaAdres().getPostcode()
				: ""));
		add(new Label("gbaWoonplaats", (IModel<Object>) () -> getModelObject().getPersoon().getGbaAdres() != null
			&& StringUtils.isNotBlank(getModelObject().getPersoon().getGbaAdres().getPlaats()) ? getModelObject().getPersoon().getGbaAdres().getPlaats() : ""));

		if (AdresUtil.isTijdelijkAdres(persoon, dateSupplier.getLocalDate()))
		{
			add(new Label("tijdelijkadres", getString("message.letop.tijdelijkadres")));
		}
		else
		{
			add(new Label("tijdelijkadres", Model.of("Nee")));
		}

		addSelectieblokkadeIndicator(client);
	}

	private void addSelectieblokkadeIndicator(Client client)
	{
		var selectieblokkadeTekst = deelnamemodusService.selectieblokkadeTekst(client);
		add(new Label("selectieblokkade", selectieblokkadeTekst).setVisible(StringUtils.isNotBlank(selectieblokkadeTekst)));
	}

}

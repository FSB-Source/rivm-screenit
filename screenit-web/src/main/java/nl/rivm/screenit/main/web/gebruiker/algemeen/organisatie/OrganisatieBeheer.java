
package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.beoordelingseenheid.AanvullendeBeGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium.AanvullendeGegevensBMHKLaboratoriumPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium.CervixLaboratoriumTarievenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium.facturatie.CervixBmhkLaboratoriumOverzichtVerrichtingenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.centraleeenheid.AanvullendeCeGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.coloscopiecentrum.ColoscopieCentrumGebiedenBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.coloscopiecentrum.ColoscopieCentrumKamerBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.coloscopielocatie.AanvullendeClGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.huisarts.AanvullendeHaGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.huisarts.facturatie.CervixHuisartsOverzichtVerrichtingenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.ifobtlab.AanvullendeLabGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.mammaAfdeling.AanvullendeMammapoliGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.mammaAfdeling.AanvullendeRadiologieGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.palab.AanvullendePaLabGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.screeningorganisatie.AanvullendeSOGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.zorginstelling.AanvullendeZiGegevensPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;

public abstract class OrganisatieBeheer extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return createContextMenu();
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.TRUE;
	}

	public static List<GebruikerMenuItem> createContextMenu()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		Instelling currentSelectedOrganisatie = ScreenitSession.get().getCurrentSelectedOrganisatie();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.zoeken", OrganisatieZoeken.class));
		if (currentSelectedOrganisatie != null && OrganisatieType.HUISARTS != currentSelectedOrganisatie.getOrganisatieType())
		{
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.gegevens", OrganisatieBasisgegevens.class));

			switch (currentSelectedOrganisatie.getOrganisatieType())
			{
			case ZORGINSTELLING:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.zi.aanvullend", AanvullendeZiGegevensPage.class));
				break;
			case COLOSCOPIELOCATIE:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.cl.aanvullend", AanvullendeClGegevensPage.class));
				break;
			case COLOSCOPIECENTRUM:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.cc.aanvullend", ColoscopieCentrumKamerBeheer.class));
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.cc.gebieden", ColoscopieCentrumGebiedenBeheer.class));
				break;
			case MAMMAPOLI:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.mm.aanvullend", AanvullendeMammapoliGegevensPage.class));
				break;
			case RADIOLOGIEAFDELING:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.rad.aanvullend", AanvullendeRadiologieGegevensPage.class));
				break;
			case SCREENINGSORGANISATIE:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.so.aanvullend", AanvullendeSOGegevensPage.class));
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.uitstrijkendartsen", GekoppeldeUitstrijkendArtsenPage.class));
				break;
			case PA_LABORATORIUM:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.palab.aanvullend", AanvullendePaLabGegevensPage.class));
				break;
			case LABORATORIUM:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.lab.aanvullend", AanvullendeLabGegevensPage.class));
				break;
			case BMHK_LABORATORIUM:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.bmhklaboratorium.aanvullend", AanvullendeGegevensBMHKLaboratoriumPage.class));
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.uitstrijkendartsen", GekoppeldeUitstrijkendArtsenPage.class));
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.bmhklaboratorium.verrichtingen", CervixBmhkLaboratoriumOverzichtVerrichtingenPage.class));
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.bmhklaboratorium.tarief", CervixLaboratoriumTarievenPage.class));
				break;
			case CENTRALE_EENHEID:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.ce.aanvullend", AanvullendeCeGegevensPage.class));
				break;
			case BEOORDELINGSEENHEID:
				contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.be.aanvullend", AanvullendeBeGegevensPage.class));
				break;
			default:
				break;
			}
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.overeenkomsten", OrganisatieOvereenkomstenPage.class));

			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.documenten", OrganisatieDocumentenPage.class));

			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.medewerkers", OrganisatieKoppelPage.class));
		}
		else if (currentSelectedOrganisatie != null && OrganisatieType.HUISARTS == currentSelectedOrganisatie.getOrganisatieType())
		{
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisatie.huisarts", AanvullendeHaGegevensPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisatie.huisarts.verrichtingen", CervixHuisartsOverzichtVerrichtingenPage.class));
		}

		return contextMenuItems;
	}
}

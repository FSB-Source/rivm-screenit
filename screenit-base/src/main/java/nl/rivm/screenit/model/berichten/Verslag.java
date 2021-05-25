package nl.rivm.screenit.model.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

public interface Verslag<T extends VerslagContent<?>, R extends ScreeningRonde> extends HibernateObject
{

	OntvangenCdaBericht getOntvangenBericht();

	void setOntvangenBericht(OntvangenCdaBericht ontvangencdaBericht);

	Instelling getUitvoerderOrganisatie();

	void setUitvoerderOrganisatie(Instelling uitvoerderOrganisatie);

	Gebruiker getUitvoerderMedewerker();

	void setUitvoerderMedewerker(Gebruiker uitvoerderMedewerker);

	Date getDatumVerwerkt();

	void setDatumVerwerkt(Date datumVerwerkt);

	InstellingGebruiker getInvoerder();

	void setInvoerder(InstellingGebruiker invoerder);

	Date getDatumOnderzoek();

	void setDatumOnderzoek(Date datumOnderzoek);

	VerslagStatus getStatus();

	void setStatus(VerslagStatus status);

	VerslagType getType();

	void setType(VerslagType type);

	R getScreeningRonde();

	void setScreeningRonde(R screeningRonde);

	T getVerslagContent();

	void setVerslagContent(T verslagContent);

}

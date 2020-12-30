package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dto.mamma.MammaUploadBeeldenVerzoekDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;

public interface MammaUploadBeeldenService
{
	List<MammaUploadBeeldenVerzoek> zoekOpenstaandeUploadBeeldenVerzoeken(Instelling instelling, ScreeningOrganisatie regio, int first, int count, SortState<String> sortState);

	long countOpenstaandeUploadBeeldenVerzoeken(Instelling instelling, ScreeningOrganisatie regio);

	List<MammaUploadBeeldenVerzoekDto> zoekInstellingenMetOpenstaandeUploadVerzoeken(ScreeningOrganisatie regio);

	void maakUploadVerzoek(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, Client client, InstellingGebruiker instellingGebruiker);

	String uploadBeelden(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, List<UploadDocument> uploadDocumenten, InstellingGebruiker loggedInInstellingGebruiker);

	void setGeenBeeldenBeschikbaar(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, InstellingGebruiker instellingGebruiker);
}

package nl.rivm.screenit.mamma.se.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.mamma.se.dto.ZorginstellingDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdRedenDto;
import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekOpslaanDto;
import nl.rivm.screenit.mamma.se.dto.actions.SignalerenOpslaanDto;
import nl.rivm.screenit.model.InstellingGebruiker;

public interface OnderzoekService
{
	void opslaan(OnderzoekOpslaanDto action, InstellingGebruiker gebruiker);

	void signalerenOpslaan(SignalerenOpslaanDto action, InstellingGebruiker account, LocalDateTime transactieDatumTijd);

	void maakDubbeleTijd(MaakDubbeleTijdDto action, InstellingGebruiker account, LocalDateTime transactieDatumTijd);

	void maakDubbeleTijdReden(MaakDubbeleTijdRedenDto action, InstellingGebruiker account);

	Map<Long, Integer> getOnderzochtByGebruikerOpDatumVoorSe(Date datum, String seCode);

	Map<Long, Integer> getAfgerondByGebruikerOpDatumVoorSe(Date datum, String seCode);

	Map<Long, Integer> getOnderbrokenByGebruikerOpDatumVoorSe(Date datum, String seCode);

	Map<Long, Integer> getOnvolledigByGebruikerOpDatumVoorSe(Date datum, String seCode);

	Map<Long, Integer> getAfwijkingenByGebruikerOpDatumVoorSe(Date datum, String seCode);

	int getAantalOnderzoekenMetBeelden(LocalDate datum, String seCode);

	int getAantalOnderzoekenMetBeeldenBeschikbaarInIms(LocalDate datum, String seCode);

	int getAantalDoorgevoerdVanDag(LocalDate datum, String seCode);

	List<ZorginstellingDto> getBKZorginstellingen();
}

package nl.rivm.screenit.main.dao.mamma;

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

import java.time.LocalDate;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Termijn;

public interface MammaLezingRapportageDao
{
	long eersteLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn);

	long tweedeLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn);

	long eersteOf2deLezerOngunstigeUitslagVervolgRondesCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn);

	long eersteOf2deLezerOngunstigeUitslagEersteRondesCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn);

	long discrepantieLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn);
}

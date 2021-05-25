package nl.rivm.screenit.mamma.se.dao;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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
import java.util.Map;

public interface MammaOnderzoekenDao
{

	Map<Long, Integer> readOnderzochtVanSeOpWerkdag(Date beginDatum, String seCode);

	Map<Long, Integer> readAfgerondVanSeOpWerkdag(Date beginDatum, String seCode);

	Map<Long, Integer> readOnderbrokenVanSeOpWerkdag(Date beginDatum, String seCode);

	Map<Long, Integer> readOnvolledigVanSeOpWerkdag(Date beginDatum, String seCode);

	Map<Long, Integer> readAfwijkingenVanSeOpWerkdag(Date beginDatum, String seCode);

	Long readAantalOnderzoekenMetBeelden(Date beginDatum, String seCode);

	Long readAantalOnderzoekenMetBeeldenBeschikbaarInIms(Date beginDatum, String seCode);

	Long readDoorgevoerdeOnderzoekenVanSeOpWerkdag(Date beginDatum, String seCode);
}

package nl.rivm.screenit.main.service.mamma;

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

import java.util.List;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;

public interface MammaPostcodeReeksService
{

	List<MammaPostcodeReeks> zoekPostcodeReeksen(MammaPostcodeReeks zoekObject, int checkedCast, int checkedCast2, String property, boolean ascending);

	long countPostcodeReeksen(MammaPostcodeReeks zoekObject);

	boolean saveOrUpdatePostcodeReeks(MammaPostcodeReeks postcodeReeks, InstellingGebruiker loggedInInstellingGebruiker);

	String overlaptBestaandeReeks(MammaPostcodeReeks postcodeReeks);

	void deletePostcodeReeks(MammaPostcodeReeks postcodeReeks, InstellingGebruiker loggedInInstellingGebruiker);

	String reeksInMeerdereRegios(MammaPostcodeReeks postcodeReeks);

}

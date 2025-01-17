package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.service.impl.PersoonCoordinaten;
import nl.topicuszorg.organisatie.model.Adres;

public interface CoordinatenService
{
	PersoonCoordinaten getCoordinatenVanPersoon(GbaPersoon persoon);

	PostcodeCoordinaten getCoordinaten(Adres adres);

	PostcodeCoordinaten getCoordinaten(String postcode, Integer huisnummer, String huisnummerToevoeging, String huisletter);

	PersoonCoordinaten getAdresEnTijdelijkAdresCoordinatenVanPersoon(GbaPersoon persoon);

	void updateGemeenteCoordinaten(String gemcode, String latitude, String longitude);

	void addOrUpdateCoordinaten(String postcode, String huisnr, String huisnummerToevoeging, String lat, String lon);
}

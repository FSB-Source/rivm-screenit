package nl.rivm.screenit.mamma.se.proxy.services;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.dicom.mpps.MppsRecord;
import nl.rivm.screenit.mamma.se.proxy.model.Amputatie;
import nl.rivm.screenit.mamma.se.proxy.model.KwaliteitsopnameScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.model.ScreenITWerklijstItem;

public interface WerklijstStoreService
{
	void setWerklijstItem(ScreenITWerklijstItem screenITWerklijstItem);

	KwaliteitsopnameScreenITWerklijstItem getActiefKwaliteitsopnameWerklijstItemByAeTitle(String aeTitle);

	ScreenITWerklijstItem getWerklijstItemVoorMammograaf(String aeTitle);

	void setActiefKwaliteitsopnameWerklijstItem(KwaliteitsopnameScreenITWerklijstItem werklijstItem);

	boolean isOpActieveMppsRecordsLijst(String uitnodigingsNr);

	boolean verwijderWerklijstItem(String aeTitle);

	void addNietAfgerondeWerklijstItem(String sopInstanceUid, MppsRecord mppsRecord);

	MppsRecord getMppsRecord(String sopInstanceUid);

	void removeNietAfgerondeWerklijstItem(String sopInstanceUid);

	boolean heeftBeeldenZijde(String accessionNumber, Amputatie zijde);

	void startOfDayCleanUp();

	void waarschuwingGecontroleerd(String accessionNumber);

	boolean verwijderActiefKwaliteitsopnameWerklijstItem(String aeTitle);
}

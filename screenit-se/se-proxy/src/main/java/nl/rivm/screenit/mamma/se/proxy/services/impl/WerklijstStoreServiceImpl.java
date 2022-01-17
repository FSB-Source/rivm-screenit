package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import nl.rivm.screenit.mamma.se.proxy.dicom.mpps.MppsRecord;
import nl.rivm.screenit.mamma.se.proxy.model.Amputatie;
import nl.rivm.screenit.mamma.se.proxy.model.KwaliteitsopnameScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.model.ScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.services.WerklijstStoreService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class WerklijstStoreServiceImpl implements WerklijstStoreService
{
	private static final Logger LOG = LoggerFactory.getLogger(WerklijstStoreServiceImpl.class);

	private Map<String, ScreenITWerklijstItem> werklijstItemsByAeTitle = new ConcurrentHashMap<>();

	private Map<String, MppsRecord> actieveMppsRecordsBySopInstanceUid = new ConcurrentHashMap<>();

	private Map<String, List<MppsRecord>> afgerondeMppsRecordsByAccessionNumber = new ConcurrentHashMap<>();

	private Map<String, Boolean> heeftAlMeldingGehadByAccessionNumber = new ConcurrentHashMap<>();

	private Map<String, KwaliteitsopnameScreenITWerklijstItem> actieveKwaliteitsopnameWerklijstItemsByAeTitle = new ConcurrentHashMap<>();

	@Override
	public void setWerklijstItem(ScreenITWerklijstItem screenITWerklijstItem)
	{
		werklijstItemsByAeTitle.put(screenITWerklijstItem.getAeTitle(), screenITWerklijstItem);
	}

	@Override
	public void setActiefKwaliteitsopnameWerklijstItem(KwaliteitsopnameScreenITWerklijstItem werklijstItem)
	{
		actieveKwaliteitsopnameWerklijstItemsByAeTitle.put(werklijstItem.getAeTitle(), werklijstItem);
	}

	@Override
	public boolean isOpActieveMppsRecordsLijst(String uitnodigingsNr)
	{
		return actieveMppsRecordsBySopInstanceUid.values().stream().anyMatch(mppsRecord -> mppsRecord.getAccessionNumber().equals(uitnodigingsNr));
	}

	@Override
	public boolean verwijderWerklijstItem(String aeTitle)
	{
		return werklijstItemsByAeTitle.remove(aeTitle) != null;
	}

	@Override
	public boolean verwijderActiefKwaliteitsopnameWerklijstItem(String aeTitle)
	{
		return actieveKwaliteitsopnameWerklijstItemsByAeTitle.remove(aeTitle) != null;
	}

	public KwaliteitsopnameScreenITWerklijstItem getActiefKwaliteitsopnameWerklijstItemByAeTitle(String aeTitle)
	{
		return actieveKwaliteitsopnameWerklijstItemsByAeTitle.get(aeTitle);
	}

	@Override
	public ScreenITWerklijstItem getWerklijstItemVoorMammograaf(String aeTitle)
	{
		return werklijstItemsByAeTitle.get(aeTitle);
	}

	@Override
	public void addNietAfgerondeWerklijstItem(String sopInstanceUid, MppsRecord mppsRecord)
	{
		actieveMppsRecordsBySopInstanceUid.put(sopInstanceUid, mppsRecord);
	}

	@Override
	public MppsRecord getMppsRecord(String sopInstanceUid)
	{
		return actieveMppsRecordsBySopInstanceUid.get(sopInstanceUid);
	}

	@Override
	public void removeNietAfgerondeWerklijstItem(String sopInstanceUid)
	{
		MppsRecord mppsRecord = actieveMppsRecordsBySopInstanceUid.remove(sopInstanceUid);
		if (afgerondeMppsRecordsByAccessionNumber.containsKey(mppsRecord.getAccessionNumber()))
		{
			afgerondeMppsRecordsByAccessionNumber.get(mppsRecord.getAccessionNumber()).add(mppsRecord);
		}
		else
		{
			afgerondeMppsRecordsByAccessionNumber.put(mppsRecord.getAccessionNumber(), new ArrayList<>(Arrays.asList(mppsRecord)));
		}
	}

	@Override
	public boolean heeftBeeldenZijde(String accessionNumber, Amputatie zijde)
	{
		if (heeftAlMeldingGehadByAccessionNumber.containsKey(accessionNumber))
		{
			return false;
		}

		List<MppsRecord> mppsRecords = afgerondeMppsRecordsByAccessionNumber.get(accessionNumber);

		if (mppsRecords == null)
		{
			return false;
		}

		for (MppsRecord mppsRecord : mppsRecords)
		{
			if (Amputatie.LINKERBORST.equals(zijde) && mppsRecord.hasBeeldenLinks())
			{
				return true;
			}
			if (Amputatie.RECHTERBORST.equals(zijde) && mppsRecord.hasBeeldenRechts())
			{
				return true;
			}
		}
		return false;
	}

	@Override
	public void startOfDayCleanUp()
	{
		LOG.info("Verwijder mammograafwerklijst administratie");
		werklijstItemsByAeTitle.clear();
		actieveMppsRecordsBySopInstanceUid.clear();
		afgerondeMppsRecordsByAccessionNumber.clear();
		heeftAlMeldingGehadByAccessionNumber.clear();
		actieveKwaliteitsopnameWerklijstItemsByAeTitle.clear();
	}

	@Override
	public void waarschuwingGecontroleerd(String accessionNumber)
	{
		heeftAlMeldingGehadByAccessionNumber.put(accessionNumber, Boolean.TRUE);
	}
}

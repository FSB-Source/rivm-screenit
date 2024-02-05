package nl.rivm.screenit.mamma.se.proxy.controller;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.model.Amputatie;
import nl.rivm.screenit.mamma.se.proxy.model.ClientScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.model.KwaliteitsopnameScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.services.WerklijstStoreService;
import nl.rivm.screenit.mamma.se.proxy.util.SafeStringUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/werklijst")
public class WerklijstProxyController
{
	@Autowired
	private WerklijstStoreService store;

	private static final Logger LOG = LoggerFactory.getLogger(WerklijstProxyController.class);

	@RequestMapping(value = "/toevoegen", method = RequestMethod.POST)
	public ResponseEntity<ClientScreenITWerklijstItem> selecteerClient(@RequestBody ClientScreenITWerklijstItem clientWerklijstItem)
	{
		store.setWerklijstItem(clientWerklijstItem);
		LOG.info("[/werklijst/toevoegen] uitnodigingsNummer: {}, ae-title: {}", clientWerklijstItem.getUitnodigingsNr(), clientWerklijstItem.getAeTitle());
		return ResponseEntity.ok(clientWerklijstItem);
	}

	@RequestMapping(value = "/startKwaliteitsopname", method = RequestMethod.POST)
	public ResponseEntity<KwaliteitsopnameScreenITWerklijstItem> startKwaliteitsopname(
		@RequestBody KwaliteitsopnameScreenITWerklijstItem werklijstItem)
	{
		KwaliteitsopnameScreenITWerklijstItem actiefItem = store.getActiefKwaliteitsopnameWerklijstItemByAeTitle(werklijstItem.getAeTitle());
		if (actiefItem != null)
		{
			LOG.warn("Station heeft al een actieve kwaliteitsopname. Geen nieuwe gestart.");
			return ResponseEntity.ok(actiefItem);
		}
		else
		{
			var safeReden = SafeStringUtil.maakStringMetUserInputVeiligVoorLogging(werklijstItem.getReden());
			var safeVoorOfNaKalibratie = SafeStringUtil.maakStringMetUserInputVeiligVoorLogging(werklijstItem.getVoorOfNaKalibratie());
			var safePatientId = SafeStringUtil.maakStringMetUserInputVeiligVoorLogging(werklijstItem.getPatientId());

			LOG.info("Start kwaliteitsopname, reden = {}, voor of na kalibratie = {}, patientId = {}, startMoment = {}", safeReden,
				safeVoorOfNaKalibratie, safePatientId, werklijstItem.getStartMoment());
			store.setActiefKwaliteitsopnameWerklijstItem(werklijstItem);
			store.setWerklijstItem(werklijstItem);
			return ResponseEntity.ok(werklijstItem);
		}
	}

	@RequestMapping(value = "/beeindigKwaliteitsopname/{aeTitle}", method = RequestMethod.DELETE)
	public ResponseEntity beeindigKwaliteitsopname(@PathVariable String aeTitle)
	{
		if (aeTitle != null)
		{
			store.verwijderWerklijstItem(aeTitle);
			store.verwijderActiefKwaliteitsopnameWerklijstItem(aeTitle);
			return ResponseEntity.ok().build();
		}
		else
		{
			return ResponseEntity.notFound().build();
		}
	}

	@RequestMapping(value = "/actieveKwaliteitsopname/{aeTitle}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public KwaliteitsopnameScreenITWerklijstItem getActieveKwaliteitsopname(@PathVariable String aeTitle)
	{
		KwaliteitsopnameScreenITWerklijstItem werklijstItem = store.getActiefKwaliteitsopnameWerklijstItemByAeTitle(aeTitle);
		if (werklijstItem != null)
		{
			return werklijstItem;
		}
		else
		{
			return new KwaliteitsopnameScreenITWerklijstItem();
		}
	}

	@RequestMapping(value = "/opActieveMppsRecordsLijst/{uitnodigingsNr}", method = RequestMethod.GET)
	public ResponseEntity isOpActieveMppsRecordsLijst(@PathVariable String uitnodigingsNr)
	{
		if (store.isOpActieveMppsRecordsLijst(uitnodigingsNr))
		{
			return ResponseEntity.ok().build();
		}
		else
		{
			return ResponseEntity.notFound().build();
		}
	}

	@RequestMapping(value = "/verwijder/{aeTitle}", method = RequestMethod.DELETE)
	public ResponseEntity<String> verwijderClient(@PathVariable String aeTitle)
	{
		store.verwijderWerklijstItem(aeTitle);
		return ResponseEntity.ok(aeTitle);
	}

	@RequestMapping(value = "/heeftBeeldenZijde/{accessionNumber}/{zijde}", method = RequestMethod.GET)
	public ResponseEntity heeftBeeldenZijde(@PathVariable String accessionNumber, @PathVariable Amputatie zijde)
	{
		if (store.heeftBeeldenZijde(accessionNumber, zijde))
		{
			return ResponseEntity.ok().build();
		}
		else
		{
			return ResponseEntity.notFound().build();
		}
	}

	@RequestMapping(value = "/waarschuwingGecontroleerd/{accessionNumber}", method = RequestMethod.POST)
	public void waarschuwingGecontroleerd(@PathVariable String accessionNumber)
	{
		store.waarschuwingGecontroleerd(accessionNumber);
	}
}

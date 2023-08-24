package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.mamma.MammaBaseIlmDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaIlmBezwaarPoging;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseIlmService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseIlmServiceImpl implements MammaBaseIlmService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseIlmServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private LogService logService;

	@Autowired
	private MammaBaseIlmDao baseIlmDao;

	@Autowired
	private MammaBaseUitwisselportaalService baseUitwisselportaalService;

	@Autowired
	private MammaBaseOnderzoekService baseOnderzoekService;

	@Override
	public void maakIlmBezwaarPoging(MammaDossier dossier, long accessionNumber, boolean isUploaded)
	{
		MammaIlmBezwaarPoging ilmBezwaarPoging = new MammaIlmBezwaarPoging();
		ilmBezwaarPoging.setAccessionNumber(accessionNumber);
		ilmBezwaarPoging.setStatusDatum(currentDateSupplier.getLocalDateTime());
		ilmBezwaarPoging.setUploaded(isUploaded);
		ilmBezwaarPoging.setDossier(dossier);
		dossier.getIlmBezwaarPogingen().add(ilmBezwaarPoging);
		hibernateService.saveOrUpdateAll(ilmBezwaarPoging, dossier);
	}

	@Override
	public boolean verwijderIlmBezwaarPoging(MammaDossier dossier, long accessionNumber)
	{
		List<MammaIlmBezwaarPoging> bezwaarPogingen = dossier.getIlmBezwaarPogingen();

		MammaIlmBezwaarPoging bezwaarPoging = bezwaarPogingen.stream().filter(entry -> entry.getAccessionNumber() == accessionNumber).findFirst().orElse(null);
		if (bezwaarPoging != null)
		{
			bezwaarPogingen.remove(bezwaarPoging);
			hibernateService.delete(bezwaarPoging);
			hibernateService.saveOrUpdate(dossier);
			return true;
		}
		return false;
	}

	@Override
	public void verwijderIlmBezwaarPogingen(MammaDossier dossier)
	{
		List<MammaIlmBezwaarPoging> bezwaarPogingen = dossier.getIlmBezwaarPogingen();
		hibernateService.deleteAll(bezwaarPogingen);
		hibernateService.saveOrUpdateAll(dossier);
	}

	@Override
	public boolean forceerIlmStatusVerwijderd(MammaIlmBeeldenStatusRapportageEntry entry, Account account)
	{
		boolean geforceerd;
		if (entry.isBezwaar())
		{
			geforceerd = forceerVerwijderIlmBezwaarPoging(entry.getClient(), entry.getAccessionNumber(), account);
		}
		else
		{
			if (entry.isUploaded())
			{
				geforceerd = baseUitwisselportaalService.forceerUploadPogingIlmStatus(entry.getAccessionNumber(), MammaMammografieIlmStatus.VERWIJDERD, account);
			}
			else
			{
				geforceerd = baseOnderzoekService.forceerMammografieIlmStatus(entry.getAccessionNumber(), MammaMammografieIlmStatus.VERWIJDERD, account);
			}
		}
		return geforceerd;
	}

	private boolean forceerVerwijderIlmBezwaarPoging(Client client, long accessionNumber, Account account)
	{
		boolean isVerwijderd = verwijderIlmBezwaarPoging(client.getMammaDossier(), accessionNumber);
		if (isVerwijderd)
		{
			String melding = String.format("AccessionNumber: %d, status: %s, isBezwaar: %b, isUploaded: %b",
				accessionNumber, MammaMammografieIlmStatus.VERWIJDERD, false, false);
			LOG.info(melding);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_ILM_STATUS_GEFORCEERD, account, client, melding, Bevolkingsonderzoek.MAMMA);
		}
		return isVerwijderd;
	}

	@Override
	public void verwijderIlmRapportageEntriesVoorClient(Client client)
	{
		List<MammaIlmBeeldenStatusRapportageEntry> entries = baseIlmDao.getRapportageEntriesVoorClient(client);
		for (MammaIlmBeeldenStatusRapportageEntry entry : entries)
		{
			MammaIlmBeeldenStatusRapportage rapportage = entry.getRapportage();
			rapportage.getEntries().remove(entry);
			hibernateService.saveOrUpdate(rapportage);
		}
		hibernateService.deleteAll(entries);
	}

}


package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dao.cervix.CervixDossierDao;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixBaseUitnodigingService;
import nl.rivm.screenit.service.cervix.CervixLabformulierService;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@AllArgsConstructor
public class CervixBaseUitnodigingServiceImpl implements CervixBaseUitnodigingService
{
	private LogService logService;

	private HibernateService hibernateService;

	private ICurrentDateSupplier dateSupplier;

	private CervixDossierDao dossierDao;

	private CervixBaseScreeningrondeService baseScreeningrondeService;

	private CervixLabformulierService labformulierService;

	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public void saveMonster(CervixZas zas, InstellingGebruiker loggedInInstellingGebruiker, String logMessage)
	{
		saveMonster(zas, !CervixZasStatus.VERSTUURD.equals(zas.getZasStatus()), loggedInInstellingGebruiker, logMessage);
	}

	@Override
	public void saveMonster(CervixUitstrijkje uitstrijkje, InstellingGebruiker loggedInInstellingGebruiker, String logMessage)
	{

		saveMonster(uitstrijkje, !CervixUitstrijkjeStatus.NIET_ONTVANGEN.equals(uitstrijkje.getUitstrijkjeStatus()), loggedInInstellingGebruiker, logMessage);
	}

	private void saveMonster(CervixMonster monster, boolean ingeboekt, InstellingGebruiker loggedInInstellingGebruiker, String logMessage)
	{
		monster.setStatusDatum(dateSupplier.getDate());
		if (ingeboekt)
		{
			if (monster.getOntvangstdatum() == null)
			{
				monster.setOntvangstdatum(dateSupplier.getDate());
				monster.setOntvangstScreeningRonde(dossierDao.getOntvangstRonde(monster));
				var bmhkLaboratorium = (BMHKLaboratorium) HibernateHelper.deproxy(loggedInInstellingGebruiker.getOrganisatie());
				monster.setLaboratorium(bmhkLaboratorium);
				labformulierService.updateLabformulierLaboratoriumNaOntvangstMonster(monster);
			}
		}
		else
		{
			if (CervixMonsterUtil.isZAS(monster) || !heeftGecontroleerdLabformulier(CervixMonsterUtil.getUitstrijkje(monster)))
			{
				monster.setOntvangstScreeningRonde(null);
			} 
			monster.setOntvangstdatum(null);
			monster.setLaboratorium(null);
			monster.setSignaleringen(null);
			monster.setOverigeSignalering(null);
		}

		hibernateService.saveOrUpdate(monster);

		Client client = monster.getUitnodiging().getScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_UITNODIGING_OPGESLAGEN, loggedInInstellingGebruiker, client, logMessage, Bevolkingsonderzoek.CERVIX);
	}

	private boolean heeftGecontroleerdLabformulier(CervixUitstrijkje uitstrijkje)
	{
		CervixLabformulier labformulier = uitstrijkje.getLabformulier();
		return labformulier != null && !(labformulier.getStatus() == CervixLabformulierStatus.GESCAND || labformulier.getStatus() == CervixLabformulierStatus.AFGEKEURD);
	}

	@Override
	public void registreerMonsterBarcodeAfgedrukt(CervixMonster monster, InstellingGebruiker loggedInInstellingGebruiker, LogGebeurtenis logGebeurtenis)
	{
		List<Date> barcodeAfgedruktList = monster.getBarcodeAfgedrukt();
		barcodeAfgedruktList.add(currentDateSupplier.getDate());
		monster.setBarcodeAfgedrukt(barcodeAfgedruktList); 
		hibernateService.saveOrUpdate(monster);
		Client client = monster.getUitnodiging().getScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(logGebeurtenis, loggedInInstellingGebruiker, client, "Monster-id: " + monster.getMonsterId(), Bevolkingsonderzoek.CERVIX);
	}

	@Override
	public void verwijderResultatenMonster(CervixMonster monster, UploadDocument uploadDocument, InstellingGebruiker loggedInInstellingGebruiker)
	{
		monster.setVerwijderdDatum(dateSupplier.getDate());
		if (uploadDocument != null)
		{
			monster.setVerwijderdBrief(uploadDocument);
		}

		CervixScreeningRonde ronde = monster.getOntvangstScreeningRonde();

		if (monster.equals(ronde.getUitstrijkjeVervolgonderzoekUitslag()))
		{
			ronde.setUitstrijkjeVervolgonderzoekUitslag(null);
		}

		if (monster.equals(ronde.getUitstrijkjeCytologieUitslag()))
		{
			ronde.setUitstrijkjeCytologieUitslag(null);
			ronde.setInVervolgonderzoekDatum(null);
		}

		if (monster.equals(ronde.getMonsterHpvUitslag()))
		{
			ronde.setMonsterHpvUitslag(null);
		}

		if (ronde.getStatus() == ScreeningRondeStatus.AFGEROND)
		{
			ronde.setStatus(ScreeningRondeStatus.LOPEND);
			ronde.setStatusDatum(dateSupplier.getDate());
		}

		if (uploadDocument != null)
		{
			hibernateService.saveOrUpdate(uploadDocument);
		}
		hibernateService.saveOrUpdateAll(monster, ronde);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_RESULTATEN_MONSTER_VERWIJDERD, loggedInInstellingGebruiker, ronde.getDossier().getClient(),
			"Monster-id: " + monster.getMonsterId(), Bevolkingsonderzoek.CERVIX);
	}

	@Override
	public CervixMonster getUitnodigingMagVerwijderdWorden(CervixScreeningRonde screeningRonde)
	{
		CervixMonster monster = null;

		if (Boolean.TRUE.equals(screeningRonde.getAangemeld()) && screeningRonde.getDossier().getLaatsteScreeningRonde().equals(screeningRonde))
		{
			if (!baseScreeningrondeService.heeftUitnodigingMetMonsterInLabproces(screeningRonde))
			{
				monster = screeningRonde.getUitstrijkjeVervolgonderzoekUitslag();
				if (monster == null)
				{
					monster = screeningRonde.getUitstrijkjeCytologieUitslag();
					if (monster == null)
					{
						monster = screeningRonde.getMonsterHpvUitslag();
					}
				}
			}
		}
		return monster;
	}

	@Override
	public void vervangVerwijderdDocument(CervixMonster monster, UploadDocument uploadDocument)
	{
		if (uploadDocument != null)
		{
			monster.setVerwijderdBrief(uploadDocument);
			hibernateService.saveOrUpdateAll(uploadDocument, monster);
		}
		else
		{
			throw new IllegalStateException("Geen verwijderd brief geupload");
		}
	}

}

package nl.rivm.screenit.service.cervix.impl;

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

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Random;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.cervix.CervixDossierDao;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.cervix.CervixBaseDossierService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.CervixTestService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.FileCopyUtils;

import com.aspose.words.Document;

@Slf4j
@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.REQUIRED)
public class CervixTestServiceImpl implements CervixTestService
{
	private final CervixBaseDossierService cervixBaseDossierService;

	private final BaseDossierService baseDossierService;

	private final TestService testService;

	private final ICurrentDateSupplier dateSupplier;

	private final ClientDao clientDao;

	private final HibernateService hibernateService;

	private final CervixFactory factory;

	private final BaseAfmeldService baseAfmeldService;

	private final AsposeService asposeService;

	private final CervixDossierDao dossierDao;

	private final DossierFactory dossierFactory;

	@Override
	public CervixDossier geefDossier(GbaPersoon gbaPersoon)
	{
		Client client = testService.maakClient(gbaPersoon);
		return client.getCervixDossier();
	}

	@Override
	public CervixScreeningRonde geefScreeningRonde(GbaPersoon gbaPersoon)
	{
		return geefScreeningRonde(gbaPersoon, null);
	}

	@Override
	public CervixScreeningRonde geefScreeningRonde(GbaPersoon gbaPersoon, Date inVervolgonderzoekDatum)
	{
		CervixDossier dossier = geefDossier(gbaPersoon);

		CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		if (ronde == null)
		{
			ronde = factory.maakRonde(dossier);
			factory.maakUitnodiging(ronde, ronde.getLeeftijdcategorie().getUitnodigingsBrief(), true, false);
			verzendLaatsteBrief(ronde);
		}
		if (inVervolgonderzoekDatum != null)
		{
			ronde.setInVervolgonderzoekDatum(inVervolgonderzoekDatum);
		}
		return ronde;
	}

	private CervixUitnodiging geefLaatsteUitnodiging(GbaPersoon gbaPersoon)
	{
		CervixScreeningRonde screeningRonde = geefScreeningRonde(gbaPersoon);
		return screeningRonde.getLaatsteUitnodiging();
	}

	@Override
	public CervixUitnodiging maakUitnodiging(GbaPersoon gbaPersoon, BriefType briefType)
	{
		CervixScreeningRonde ronde = geefScreeningRonde(gbaPersoon);

		CervixUitnodiging uitnodiging = factory.maakUitnodiging(ronde, briefType, true, true);
		verzendLaatsteBrief(ronde);
		return uitnodiging;
	}

	@Override
	public CervixUitstrijkje geefUitstrijkje(GbaPersoon gbaPersoon, CervixUitstrijkjeStatus uitstrijkjeStatus, String monsterId,
		BMHKLaboratorium bmhkLaboratorium)
	{
		CervixUitnodiging uitnodiging = geefLaatsteUitnodiging(gbaPersoon);

		CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) HibernateHelper.deproxy(uitnodiging.getMonster());
		if (uitstrijkjeStatus != null)
		{
			uitstrijkje.setUitstrijkjeStatus(uitstrijkjeStatus);
			uitstrijkje.setStatusDatum(dateSupplier.getDate());
		}
		if (uitstrijkjeStatus == CervixUitstrijkjeStatus.ONTVANGEN)
		{
			uitstrijkje.setOntvangstdatum(dateSupplier.getDate());
			uitstrijkje.setOntvangstScreeningRonde(dossierDao.getOntvangstRonde(uitstrijkje));
			uitstrijkje.setLaboratorium(bmhkLaboratorium);
		}
		return uitstrijkje;
	}

	@Override
	public CervixLabformulier geefLabformulier(GbaPersoon gbaPersoon, CervixLabformulierStatus labformulierStatus, BMHKLaboratorium laboratorium,
		CervixHuisartsLocatie huisartsLocatie)
	{
		CervixUitstrijkje uitstrijkje = geefUitstrijkje(gbaPersoon, null, null, null);

		CervixLabformulier labformulier = uitstrijkje.getLabformulier();
		if (labformulier == null)
		{
			labformulier = maakLabformulier(uitstrijkje, labformulierStatus, laboratorium);
		}
		else
		{
			if (labformulierStatus != null)
			{
				labformulier.setStatus(labformulierStatus);
				labformulier.setStatusDatum(dateSupplier.getDate());
			}
		}
		if (huisartsLocatie != null)
		{
			labformulier.setHuisartsLocatie(huisartsLocatie);
			hibernateService.saveOrUpdate(labformulier);
		}
		return labformulier;
	}

	private CervixLabformulier maakLabformulier(CervixUitstrijkje uitstrijkje, CervixLabformulierStatus labformulierStatus, BMHKLaboratorium laboratorium)
	{
		if (laboratorium == null)
		{
			laboratorium = hibernateService.loadAll(BMHKLaboratorium.class).get(0);
		}

		CervixLabformulier labformulier = new CervixLabformulier();
		labformulier.setUitstrijkje(uitstrijkje);
		labformulier.setLaboratorium(laboratorium);
		labformulier.setScanDatum(dateSupplier.getDate());
		labformulier.setObjid(Long.toString(Math.abs(new Random().nextInt())));
		labformulier.setStatus(labformulierStatus);
		labformulier.setStatusDatum(dateSupplier.getDate());
		labformulier.setBarcode(uitstrijkje.getMonsterId());
		labformulier.setDatumUitstrijkje(DateUtil.toUtilDate(dateSupplier.getLocalDate().minusDays(1)));
		hibernateService.saveOrUpdate(labformulier);

		uitstrijkje.setLabformulier(labformulier);
		hibernateService.saveOrUpdate(uitstrijkje);
		return labformulier;
	}

	@Override
	public Document geefBarcodeUitnodigingsIdTestPdf(CervixUitnodiging uitnodiging)
	{
		Document document = null;
		MailMergeContext context = new MailMergeContext();
		context.setCervixUitnodiging(uitnodiging);
		context.setClient(uitnodiging.getScreeningRonde().getDossier().getClient());

		try (InputStream inputStream = getClass().getResourceAsStream("/CervixUitnodigingsSticker.doc"))
		{
			byte[] briefTemplateBytes = FileCopyUtils.copyToByteArray(inputStream);
			document = asposeService.processDocument(briefTemplateBytes, context);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage());
		}
		return document;
	}

	private void verzendLaatsteBrief(CervixScreeningRonde ronde)
	{
		CervixBrief brief = ronde.getLaatsteBrief();

		CervixMergedBrieven mergedBrieven = new CervixMergedBrieven();
		mergedBrieven.setCreatieDatum(dateSupplier.getDate());
		mergedBrieven.setBriefType(ronde.getLeeftijdcategorie().getUitnodigingsBrief());
		mergedBrieven.setBrieven(new ArrayList<>());
		mergedBrieven.setPrintDatum(dateSupplier.getDate());
		mergedBrieven.setVerwijderd(true);
		brief.setMergedBrieven(mergedBrieven);
		UploadDocument fakeMergeDocument = new UploadDocument();
		fakeMergeDocument.setActief(true);
		fakeMergeDocument.setNaam("dummy_testservice_brief_niet_openen");
		hibernateService.saveOrUpdate(fakeMergeDocument);
		mergedBrieven.setMergedBrieven(fakeMergeDocument);
		hibernateService.saveOrUpdate(mergedBrieven);
		hibernateService.saveOrUpdate(brief);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public String clientenResetten(String bsns)
	{
		int gevondenEnIsVerwijderd = 0;
		String[] bsnList = bsns.split(",");
		String result = "Succesvol";
		for (String bsn : bsnList)
		{
			try
			{
				if (StringUtils.isBlank(bsn) || bsn.trim().length() != 9)
				{
					continue;
				}
				Client client = clientDao.getClientByBsn(bsn.trim());
				if (client == null)
				{
					continue;
				}
				clientReset(client);
				testService.verwijderClientContacten(client, Bevolkingsonderzoek.CERVIX);
				gevondenEnIsVerwijderd++;
			}
			catch (Exception e)
			{
				result = "Fout bij resetten van client met BSN " + bsn;
				LOG.error("error bij bsn " + bsn, e);
			}
		}
		return result + ". #" + gevondenEnIsVerwijderd + " clienten gereset.";
	}

	@Override
	public void clientReset(Client client)
	{
		CervixDossier dossier = client.getCervixDossier();

		if (dossier == null)
		{
			return;
		}
		cervixBaseDossierService.maakDossierLeeg(dossier);

		baseDossierService.verwijderLaatsteAfmelding(dossier);

		List<CervixBrief> overgeblevenBrieven = hibernateService.getByParameters(CervixBrief.class, Map.of("client", client));
		hibernateService.deleteAll(overgeblevenBrieven);

		client.setCervixDossier(null);
		hibernateService.delete(dossier);
		hibernateService.saveOrUpdate(client);

		dossierFactory.maakDossiers(client);
		LOG.info("Client gereset met bsn: " + client.getPersoon().getBsn());
	}

	@Override
	public int clientenDefinitiefAfmelden(List<Client> clienten, CervixAfmeldingReden afmeldingReden)
	{
		int aantalAfgemeld = 0;
		for (Client client : clienten)
		{
			CervixCISHistorie cisHistorie = client.getCervixDossier().getCisHistorie();
			CervixAfmelding afmelding = new CervixAfmelding();
			afmelding.setDossier(client.getCervixDossier());
			afmelding.setType(AfmeldingType.DEFINITIEF);
			afmelding.setManier(ClientContactManier.DIRECT);
			afmelding.setReden(afmeldingReden);
			baseAfmeldService.afmeldenZonderVervolg(client, afmelding, false, null);
			if (cisHistorie != null)
			{
				cisHistorie.setAfmelding(afmelding);
				hibernateService.saveOrUpdate(cisHistorie);
			}
			aantalAfgemeld++;
		}
		return aantalAfgemeld;
	}
}

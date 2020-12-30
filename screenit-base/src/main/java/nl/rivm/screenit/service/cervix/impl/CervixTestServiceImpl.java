package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Random;

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.cervix.CervixDossierDao;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.CervixTestService;
import nl.rivm.screenit.service.impl.ImportBvoViaCsv;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixTestServiceImpl implements CervixTestService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixTestServiceImpl.class);

	@Autowired
	private TestService testService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private ClientDao clientDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CervixFactory factory;

	@Autowired
	private ClientService clientService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private CervixDossierDao dossierDao;

	@Autowired
	private CervixBaseScreeningrondeService cervixBaseScreeningrondeService;

	@Autowired
	private DossierFactory dossierFactory;

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
			factory.maakUitnodiging(ronde, BriefType.CERVIX_UITNODIGING, true, false);
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

	private CervixUitnodiging geefLaatsteZasUitnodgiging(GbaPersoon gbaPersoon)
	{
		CervixScreeningRonde screeningRonde = geefScreeningRonde(gbaPersoon);
		List<CervixUitnodiging> uitnodigingen = screeningRonde.getUitnodigingen();
		for (CervixUitnodiging uitnodiging : uitnodigingen)
		{
			if (CervixMonsterType.ZAS.equals(uitnodiging.getMonsterType()))
			{
				return uitnodiging;
			}
		}
		return null;
	}

	@Override
	public CervixUitnodiging maakUitnodiging(GbaPersoon gbaPersoon, BriefType briefType)
	{
		CervixScreeningRonde ronde = geefScreeningRonde(gbaPersoon);

		CervixUitnodiging uitnodiging = factory.maakUitnodiging(ronde, briefType, true, true);
		verzendLaatsteBrief(ronde);
		return uitnodiging;
	}

	public CervixMonster geefMonster(GbaPersoon gbaPersoon)
	{
		return geefLaatsteUitnodiging(gbaPersoon).getMonster();

	}

	@Override
	public CervixUitstrijkje geefUitstrijkje(GbaPersoon gbaPersoon)
	{
		return geefUitstrijkje(gbaPersoon, null, null);
	}

	@Override
	public CervixUitstrijkje geefUitstrijkje(GbaPersoon gbaPersoon, String monsterId)
	{
		return geefUitstrijkje(gbaPersoon, null, monsterId, null);
	}

	@Override
	public CervixUitstrijkje geefUitstrijkje(GbaPersoon gbaPersoon, CervixUitstrijkjeStatus uitstrijkjeStatus, BMHKLaboratorium laboratorium)
	{
		return geefUitstrijkje(gbaPersoon, uitstrijkjeStatus, null, laboratorium);
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
	public CervixZas geefZas(GbaPersoon gbaPersoon, CervixZasStatus zasStatus, BMHKLaboratorium bmhkLaboratorium)
	{
		return geefZas(gbaPersoon, zasStatus, null, bmhkLaboratorium);
	}

	@Override
	public CervixZas geefZas(GbaPersoon gbaPersoon, CervixZasStatus zasStatus, String monsterId, BMHKLaboratorium bmhkLaboratorium)
	{
		CervixUitnodiging uitnodiging = geefLaatsteZasUitnodgiging(gbaPersoon);
		if (uitnodiging == null)
		{
			uitnodiging = maakUitnodiging(gbaPersoon, BriefType.CERVIX_ZAS_UITNODIGING);
		}

		CervixZas zas = (CervixZas) HibernateHelper.deproxy(uitnodiging.getMonster());
		if (zasStatus != null)
		{
			zas.setZasStatus(zasStatus);
			zas.setStatusDatum(dateSupplier.getDate());
		}
		if (zasStatus == CervixZasStatus.ONTVANGEN)
		{
			zas.setOntvangstdatum(dateSupplier.getDate());
			zas.setOntvangstScreeningRonde(dossierDao.getOntvangstRonde(zas));
			zas.setLaboratorium(bmhkLaboratorium);
		}
		return zas;
	}

	@Override
	public CervixHpvUitslag geefHpvUitslag(GbaPersoon gbaPersoon, CervixHpvUitslag hpvUitslag, BMHKLaboratorium laboratorium)
	{
		CervixHpvBericht hpvBericht = factory.maakHpvBericht(laboratorium, "instrumentId", "hl7Bericht", Long.toString(System.currentTimeMillis()));
		hpvBericht.setStatus(BerichtStatus.VERWERKT);
		hibernateService.saveOrUpdate(hpvBericht);

		CervixUitnodiging uitnodiging = geefLaatsteUitnodiging(gbaPersoon);
		factory.maakHpvBeoordeling(uitnodiging.getMonster(), hpvBericht, dateSupplier.getDate(), dateSupplier.getDate(), hpvUitslag);

		if (hpvUitslag != CervixHpvUitslag.ONGELDIG)
		{
			CervixScreeningRonde ronde = geefScreeningRonde(gbaPersoon);
			ronde.setMonsterHpvUitslag(uitnodiging.getMonster());
			hibernateService.saveOrUpdate(ronde);
		}
		return hpvUitslag;
	}

	@Override
	public CervixUitstrijkje geefCytologieUitslag(GbaPersoon gbaPersoon, CervixCytologieUitslag cytologieUitslag, BMHKLaboratorium laboratorium)
	{
		CervixUitstrijkje uitstrijkje = cytologieUitslag(gbaPersoon, cytologieUitslag, laboratorium);
		CervixScreeningRonde ontvangstRonde = uitstrijkje.getOntvangstScreeningRonde();
		if (cytologieUitslag != CervixCytologieUitslag.PAP0)
		{
			ontvangstRonde.setUitstrijkjeCytologieUitslag(uitstrijkje);
		}

		hibernateService.saveOrUpdate(ontvangstRonde);
		return uitstrijkje;
	}

	@Override
	public CervixUitstrijkje geefVervolgonderzoekUitslag(GbaPersoon gbaPersoon, CervixCytologieUitslag cytologieUitslag,
		BMHKLaboratorium laboratorium)
	{
		CervixUitstrijkje uitstrijkje = cytologieUitslag(gbaPersoon, cytologieUitslag, laboratorium);
		CervixScreeningRonde ontvangstRonde = uitstrijkje.getOntvangstScreeningRonde();
		if (cytologieUitslag != CervixCytologieUitslag.PAP0)
		{
			ontvangstRonde.setUitstrijkjeVervolgonderzoekUitslag(uitstrijkje);
		}
		hibernateService.saveOrUpdate(ontvangstRonde);
		return uitstrijkje;
	}

	private CervixUitstrijkje cytologieUitslag(GbaPersoon gbaPersoon, CervixCytologieUitslag cytologieUitslag, BMHKLaboratorium laboratorium)
	{
		CervixUitstrijkje uitstrijkje = geefUitstrijkje(gbaPersoon);
		CervixScreeningRonde ontvangstRonde = uitstrijkje.getOntvangstScreeningRonde();

		OntvangenCdaBericht ontvangenCdaBericht = new OntvangenCdaBericht();
		ontvangenCdaBericht.setOntvangen(dateSupplier.getDate());
		ontvangenCdaBericht.setBerichtType(BerichtType.CERVIX_CYTOLOGIE_VERSLAG);
		ontvangenCdaBericht.setStatus(BerichtStatus.VERWERKT);

		CervixCytologieVerslag cytologieVerslag = new CervixCytologieVerslag();
		cytologieVerslag.setType(VerslagType.CERVIX_CYTOLOGIE);
		cytologieVerslag.setStatus(VerslagStatus.AFGEROND);
		cytologieVerslag.setCytologieUitslag(cytologieUitslag);
		cytologieVerslag.setOntvangenBericht(ontvangenCdaBericht);
		cytologieVerslag.setLaboratorium(laboratorium);
		cytologieVerslag.setUitstrijkje(uitstrijkje);
		uitstrijkje.setCytologieVerslag(cytologieVerslag);
		cytologieVerslag.setScreeningRonde(ontvangstRonde);
		ontvangstRonde.getVerslagen().add(cytologieVerslag);

		CervixCytologieVerslagContent cytologieVerslagContent = new CervixCytologieVerslagContent();
		cytologieVerslag.setVerslagContent(cytologieVerslagContent);
		cytologieVerslagContent.setVerslag(cytologieVerslag);

		hibernateService.saveOrUpdate(ontvangenCdaBericht);
		hibernateService.saveOrUpdate(cytologieVerslagContent);
		hibernateService.saveOrUpdate(cytologieVerslag);
		hibernateService.saveOrUpdate(uitstrijkje);
		hibernateService.saveOrUpdate(ontvangstRonde);
		return uitstrijkje;
	}

	@Override
	public CervixLabformulier geefLabformulier(GbaPersoon gbaPersoon, CervixLabformulierStatus labformulierStatus, BMHKLaboratorium laboratorium,
		CervixHuisartsLocatie huisartsLocatie)
	{
		CervixUitstrijkje uitstrijkje = geefUitstrijkje(gbaPersoon);

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
		labformulier.setDatumUitstrijkje(dateSupplier.getDateTime().minusDays(1).toDate());
		hibernateService.saveOrUpdate(labformulier);

		uitstrijkje.setLabformulier(labformulier);
		hibernateService.saveOrUpdate(uitstrijkje);
		return labformulier;
	}

	@Override
	public CervixCytologieOrder geefCytologieOrder(GbaPersoon gbaPersoon)
	{
		CervixUitstrijkje uitstrijkje = geefUitstrijkje(gbaPersoon);
		CervixCytologieOrder cytologieOrder = factory.maakCytologieOrder(uitstrijkje, CervixCytologieReden.INITIEEL_ZONDER_ZAS, "hl7Bericht");
		return cytologieOrder;
	}

	@Override
	public CervixHuisartsLocatie geefHuisartsLocatie()
	{
		return (CervixHuisartsLocatie) hibernateService.getHibernateSession().createCriteria(CervixHuisartsLocatie.class).setMaxResults(1)
			.setFirstResult(0).uniqueResult();
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void importCervixClientenViaCsv(File file) throws IOException, ParseException
	{
		testService.importClientenViaCsv(file, new ImportBvoViaCsv()
		{
			@Override
			protected void importBvoViaCsv(Map<String, Integer> headersIndex, String[] columns, Client client)
			{
				BMHKLaboratorium laboratorium = hibernateService.loadAll(BMHKLaboratorium.class).get(0);
				String creatieRonde = columns[headersIndex.get("CreatieRonde")];
				if (StringUtils.isNotBlank(creatieRonde))
				{
					DateTime datumCreatieEersteRonde = dateSupplier.getDateTime().plusDays(Integer.parseInt(creatieRonde));

					CervixDossier dossier = client.getCervixDossier();

					CervixScreeningRonde screeningRonde = new CervixScreeningRonde();
					screeningRonde.setCreatieDatum(datumCreatieEersteRonde.toDate());
					screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
					screeningRonde.setAangemeld(Boolean.TRUE);
					screeningRonde.setDossier(dossier);
					ArrayList<CervixScreeningRonde> screeningRondes = new ArrayList<>();
					screeningRondes.add(screeningRonde);
					dossier.setScreeningRondes(screeningRondes);
					dossier.setLaatsteScreeningRonde(screeningRonde);

					CervixUitnodiging uitnodiging = new CervixUitnodiging();
					uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
					uitnodiging.setCreatieDatum(datumCreatieEersteRonde.plusDays(1).toDate());
					uitnodiging.setUitnodigingsDatum(datumCreatieEersteRonde.plusDays(2).toDate());
					uitnodiging.setMonsterType(CervixMonsterType.UITSTRIJKJE);
					uitnodiging.setScreeningRonde(screeningRonde);
					List<CervixUitnodiging> uitnodigingen = new ArrayList<>();
					uitnodigingen.add(uitnodiging);
					screeningRonde.setUitnodigingen(uitnodigingen);

					CervixMergedBrieven mergedBrieven = new CervixMergedBrieven();
					mergedBrieven.setPrintDatum(datumCreatieEersteRonde.plusDays(2).toDate());
					mergedBrieven.setCreatieDatum(datumCreatieEersteRonde.plusDays(2).toDate());
					mergedBrieven.setBriefType(BriefType.CERVIX_UITNODIGING);
					CervixBrief brief = new CervixBrief();
					brief.setBriefType(BriefType.CERVIX_UITNODIGING);
					brief.setCreatieDatum(datumCreatieEersteRonde.plusDays(2).toDate());
					brief.setMergedBrieven(mergedBrieven);
					brief.setClient(client);
					hibernateService.saveOrUpdate(brief);
					uitnodiging.setBrief(brief);

					String uitstrijkjeStatus = columns[headersIndex.get("UitstrijkjeStatus")];
					CervixUitstrijkje uitstrijkje = new CervixUitstrijkje();
					if (StringUtils.isNotBlank(uitstrijkjeStatus))
					{
						uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.valueOf(uitstrijkjeStatus));
						uitstrijkje.setLaboratorium(hibernateService.loadAll(BMHKLaboratorium.class).get(0));
					}
					else
					{
						uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.NIET_ONTVANGEN);
					}
					uitstrijkje.setStatusDatum(datumCreatieEersteRonde.plusDays(21).toDate());
					uitstrijkje.setUitnodiging(uitnodiging);
					uitnodiging.setMonster(uitstrijkje);
					uitstrijkje.setMonsterId(Long.toString(Math.abs(new Random().nextInt())));

					hibernateService.saveOrUpdate(uitstrijkje);

					String hpvUitslag = columns[headersIndex.get("HPVUitslag")];
					if (StringUtils.isNotBlank(hpvUitslag))
					{
						CervixHpvBeoordeling beoordeling = new CervixHpvBeoordeling();
						beoordeling.setMonster(uitstrijkje);
						beoordeling.setHpvUitslag(CervixHpvUitslag.valueOf(hpvUitslag));
						beoordeling.setAnalyseDatum(datumCreatieEersteRonde.plus(23).toDate());
						beoordeling.setAutorisatieDatum(datumCreatieEersteRonde.plus(23).toDate());
						uitstrijkje.setLaatsteHpvBeoordeling(beoordeling);

						uitstrijkje.setStatusDatum(datumCreatieEersteRonde.plusDays(22).toDate());
						if (uitstrijkje.getLaatsteHpvBeoordeling().getHpvUitslag() != CervixHpvUitslag.ONGELDIG)
						{
							screeningRonde.setMonsterHpvUitslag(uitstrijkje);
						}
						hibernateService.saveOrUpdate(beoordeling);
					}

					String labformulierStatus = columns[headersIndex.get("LabformulierStatus")];
					if (StringUtils.isNotBlank(labformulierStatus))
					{
						CervixLabformulierStatus cervixLabformulierStatus = CervixLabformulierStatus.valueOf(labformulierStatus);

						CervixLabformulier labformulier = new CervixLabformulier();
						Date scanDatum = datumCreatieEersteRonde.plusDays(20).toDate();
						labformulier.setScanDatum(scanDatum);
						labformulier.setStatusDatum(scanDatum);
						labformulier.setObjid(columns[headersIndex.get("ObjId")]);
						labformulier.setStatus(cervixLabformulierStatus);
						labformulier.setLaboratorium(laboratorium);

						String uitnodigingGevonden = columns[headersIndex.get("UitnodigingGevonden")];
						if (!(cervixLabformulierStatus.equals(CervixLabformulierStatus.GESCAND)
							|| cervixLabformulierStatus.equals(CervixLabformulierStatus.AFGEKEURD))
							|| StringUtils.isNotBlank(uitnodigingGevonden))
						{
							labformulier.setBarcode("UITNODIGING_ID");
							uitstrijkje.setLabformulier(labformulier);
							labformulier.setUitstrijkje(uitstrijkje);
						}
						else
						{

							hibernateService.saveOrUpdate(labformulier);
						}
					}
				}
			}
		});

		List<CervixLabformulier> labformulieren = hibernateService.loadAll(CervixLabformulier.class);
		for (CervixLabformulier labformulier : labformulieren)
		{
			if ("UITNODIGING_ID".equals(labformulier.getBarcode()))
			{
				labformulier.setBarcode(labformulier.getUitstrijkje().getMonsterId());
			}
		}
	}

	@Override
	public Document geefBarcodeUitnodigingsIdTestPdf(CervixUitnodiging uitnodiging)
	{
		Document document = null;
		MailMergeContext context = new MailMergeContext();
		context.setCervixUitnodiging(uitnodiging);
		context.setClient(uitnodiging.getScreeningRonde().getDossier().getClient());
		try
		{
			File briefTemplate = new File(getClass().getClassLoader().getResource("CervixUitnodigingsSticker.doc").toURI());
			byte[] briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
			document = asposeService.processDocument(briefTemplateBytes, context);
		}
		catch (IOException e)
		{
			LOG.error(e.getMessage());
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
		mergedBrieven.setBriefType(BriefType.CERVIX_UITNODIGING);
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
		CervixCISHistorie cisHistorie = dossier.getCisHistorie();
		if (cisHistorie != null)
		{
			verwijderCisHistorie(cisHistorie);
		}

		cervixBaseScreeningrondeService.verwijderCervixScreeningRondes(dossier);

		if (CollectionUtils.isNotEmpty(dossier.getAfmeldingen()))
		{
			for (CervixAfmelding afmelding : dossier.getAfmeldingen())
			{
				afmelding.setAfmeldingAanvraag(null);
				afmelding.setAfmeldingBevestiging(null);
				afmelding.setHeraanmeldAanvraag(null);
				afmelding.setHeraanmeldBevestiging(null);
				hibernateService.deleteAll(afmelding.getBrieven());
			}
			dossier.setLaatsteAfmelding(null);
			hibernateService.deleteAll(dossier.getAfmeldingen());
		}

		client.setCervixDossier(null);
		hibernateService.delete(dossier);
		hibernateService.saveOrUpdate(client);

		dossierFactory.maakDossiers(client);

		LOG.info("Client gereset met bsn: " + client.getPersoon().getBsn());
	}

	private void verwijderCisHistorie(CervixCISHistorie cisHistorie)
	{
		cisHistorie.setScreeningRonde(null);
		cisHistorie.setAfmelding(null);
		cisHistorie.setUitstel(null);
		hibernateService.saveOrUpdate(cisHistorie);
		hibernateService.delete(cisHistorie);
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
			clientService.afmeldenZonderVervolg(client, afmelding, false, null);
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

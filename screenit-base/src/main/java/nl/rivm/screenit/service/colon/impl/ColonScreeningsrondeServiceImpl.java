package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.IFobtService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonScreeningsrondeServiceImpl implements ColonScreeningsrondeService
{
	private static final Logger LOG = LoggerFactory.getLogger(ColonScreeningsrondeServiceImpl.class);

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ClientDao clientDao;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private LogService logService;

	@Autowired
	@Lazy
	private IFobtService iFobtService;

	@Override
	public boolean heeftUitslag(ColonUitnodiging uitnodiging, boolean checkAlleenUitslagGecommuiceerd)
	{
		ColonScreeningRonde screeningRonde = uitnodiging.getScreeningRonde();
		for (ColonBrief brief : screeningRonde.getBrieven())
		{
			if (BriefUtil.isOngunstigeUitslagBrief(brief))
			{
				return true;
			}
			else if (brief.getBriefType() == BriefType.COLON_GUNSTIGE_UITSLAG)
			{
				IFOBTTest laatsteIFOBTTest = screeningRonde.getLaatsteIFOBTTest();
				IFOBTTest laatsteIFOBTTestExtra = screeningRonde.getLaatsteIFOBTTestExtra();
				if (laatsteIFOBTTest != null && laatsteIFOBTTest.getStatus() != IFOBTTestStatus.VERWIJDERD)
				{
					return true;
				}
				if (laatsteIFOBTTestExtra != null && laatsteIFOBTTestExtra.getStatus() != IFOBTTestStatus.VERWIJDERD)
				{
					return true;
				}
			}
		}
		if (!checkAlleenUitslagGecommuiceerd)
		{
			IFOBTTest gekoppeldeTest = uitnodiging.getGekoppeldeTest();
			IFOBTTest gekoppeldeTestExtra = uitnodiging.getGekoppeldeExtraTest();

			return gekoppeldeTestExtra != null && gekoppeldeTestExtra.getUitslag() != null || gekoppeldeTest != null && gekoppeldeTest.getUitslag() != null
				|| uitnodiging.getAntwoordFormulier() != null;
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public ColonUitnodiging createNieuweUitnodiging(ColonScreeningRonde ronde, ColonUitnodigingCategorie nieuweUitnodigingCategorie)
	{
		ColonUitnodiging uitnodiging = null;
		ColonUitnodiging laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		if (laatsteUitnodiging != null && heeftUitslag(laatsteUitnodiging, true))
		{
			LOG.warn("Geen nieuwe uitnodiging aangemaakt, omdat er al een uitslag in de ronde aanwezig is. UID: " + laatsteUitnodiging.getUitnodigingsId());
		}
		else
		{
			LocalDateTime nu = currentDateSupplier.getLocalDateTime();
			uitnodiging = new ColonUitnodiging();
			uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
			uitnodiging.setOnderzoeksVariant(ColonOnderzoeksVariant.STANDAARD);
			if (laatsteUitnodiging == null)
			{
				if (ronde.getDossier().getScreeningRondes().size() == 1)
				{

					nieuweUitnodigingCategorie = ColonUitnodigingCategorie.U1;
				}
				else
				{
					nieuweUitnodigingCategorie = ColonUitnodigingCategorie.U2;
				}
			}
			uitnodiging.setColonUitnodigingCategorie(nieuweUitnodigingCategorie);
			uitnodiging.setScreeningRonde(ronde);
			uitnodiging.setCreatieDatum(DateUtil.toUtilDate(nu.plusSeconds(1)));
			Integer vooraankondigingsperiode = preferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
			Date nieuweUitnodigingsDatum = DateUtil.toUtilDate(DateUtil.toLocalDate(uitnodiging.getCreatieDatum()).plusDays(1));
			Date minUitnodigingsDatum = DateUtil.toUtilDate(DateUtil.toLocalDateTime(ronde.getCreatieDatum()).plusDays(vooraankondigingsperiode));
			if (nieuweUitnodigingsDatum.before(minUitnodigingsDatum))
			{
				nieuweUitnodigingsDatum = minUitnodigingsDatum;
			}

			if (laatsteUitnodiging != null)
			{
				if (!laatsteUitnodiging.isVerstuurd())
				{

					ronde.getUitnodigingen().remove(laatsteUitnodiging);
					nieuweUitnodigingsDatum = laatsteUitnodiging.getUitnodigingsDatum();
					ronde.setLaatsteUitnodiging(null);
					hibernateService.delete(laatsteUitnodiging);
				}
				else
				{
					IFOBTTest test = IFOBTTestUtil.getIfobtTest(laatsteUitnodiging);
					iFobtService.setTestenVerlorenIndienActief(test);
				}
			}
			nieuweUitnodigingsDatum = fixUitnodigingsDatumBij2OpEenAdres(nieuweUitnodigingsDatum, uitnodiging);
			uitnodiging.setUitnodigingsDatum(nieuweUitnodigingsDatum);
			ronde.setLaatsteUitnodiging(uitnodiging);
			ronde.setLaatsteIFOBTTest(null);
			ronde.setLaatsteIFOBTTestExtra(null);
			ronde.getUitnodigingen().add(uitnodiging);

			hibernateService.saveOrUpdate(uitnodiging);
			hibernateService.saveOrUpdate(ronde);
		}
		return uitnodiging;
	}

	private Date fixUitnodigingsDatumBij2OpEenAdres(Date nieuweUitnodigingsDatum, ColonUitnodiging uitnodiging)
	{
		Integer uitnodigingsInterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsInterval == null)
		{
			throw new IllegalStateException("Spreidingsperiode op de parameterisatie pagina is niet gezet");
		}
		Integer minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		if (minimaleLeeftijd == null)
		{
			throw new IllegalStateException("Minimale leeftijd colonscreening op de parameterisatie pagina is niet gezet.");
		}

		Integer maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		if (maximaleLeeftijd == null)
		{
			throw new IllegalStateException("Maximale leeftijd colonscreening op de parameterisatie pagina is niet gezet");
		}
		Integer wachttijdVerzendenPakket = preferenceService.getInteger(PreferenceKey.WACHTTIJD_VERZENDEN_PAKKET_TWEE_OP_EEN_ADRES.name());
		if (wachttijdVerzendenPakket == null)
		{
			throw new IllegalStateException("Wachttijd verzenden pakket bij 2 op 1 adres op de parameterisatie pagina is niet gezet");
		}
		Client client = uitnodiging.getScreeningRonde().getDossier().getClient();
		List<Client> clientenOpAdres = clientDao.getClientenOpAdres(client.getPersoon().getGbaAdres(), minimaleLeeftijd,
			maximaleLeeftijd, uitnodigingsInterval);
		Client andereClient = ColonRestrictions.getAndereClient(clientenOpAdres, client);
		boolean andereClientMetZelfdeAdresHeeftActiveIfobt = clientenOpAdres.size() == 2 && ColonRestrictions.isIfobtActief(andereClient, new ArrayList<>())
			&& !ColonRestrictions.isWachttijdOpPakketVerstreken(andereClient, wachttijdVerzendenPakket, new ArrayList<>(), currentDateSupplier.getLocalDate());

		if (andereClientMetZelfdeAdresHeeftActiveIfobt)
		{
			LocalDate creatieDatumUitnodigingAndereClient = DateUtil
				.toLocalDate(andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getCreatieDatum());
			creatieDatumUitnodigingAndereClient = creatieDatumUitnodigingAndereClient.plusDays(wachttijdVerzendenPakket);
			if (nieuweUitnodigingsDatum.before(DateUtil.toUtilDate(creatieDatumUitnodigingAndereClient)))
			{
				nieuweUitnodigingsDatum = DateUtil.toUtilDate(creatieDatumUitnodigingAndereClient);
			}
		}
		return nieuweUitnodigingsDatum;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean maakGunstigeUitslagBriefVoorLaatsteRonde(Client client)
	{
		ColonScreeningRonde csr = client.getColonDossier().getLaatsteScreeningRonde();
		IFOBTTest eersteGunstigeUitslag = ColonScreeningRondeUtil.getEersteGunstigeTest(csr);
		if (eersteGunstigeUitslag != null && !ColonScreeningRondeUtil.zijnErOngunstigeIfobts(csr))
		{
			ColonBrief brief = briefService.maakBvoBrief(csr, BriefType.COLON_GUNSTIGE_UITSLAG);
			brief.setIfobtTest(eersteGunstigeUitslag);
			hibernateService.saveOrUpdate(brief);

			if (!ColonScreeningRondeUtil.zijnErActieveIfobts(csr))
			{
				csr.setStatus(ScreeningRondeStatus.AFGEROND);
				csr.setStatusDatum(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(csr);
			}
			logService.logGebeurtenis(LogGebeurtenis.GUNSTIGE_UITSLAG_VERSTUURD, client, Bevolkingsonderzoek.COLON);
			return true;
		}
		return false;
	}

	@Override
	public boolean isRondeStatusBuitenDoelgroep(ColonScreeningRonde ronde)
	{
		Integer uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		Integer maxLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		LocalDate creatieDatumRonde = DateUtil.toLocalDate(ronde.getCreatieDatum());
		LocalDate vandaag = currentDateSupplier.getLocalDate();
		LocalDate geboortedatum = DateUtil.toLocalDate(ronde.getDossier().getClient().getPersoon().getGeboortedatum());

		return creatieDatumRonde.plusDays(uitnodigingsinterval).isBefore(vandaag) && geboortedatum.plusYears(maxLeeftijd).plusYears(1).isBefore(vandaag);
	}

	@Override
	public boolean heeftMaxAantalFitAanvragenBereikt(ColonScreeningRonde laatsteScreeningRonde)
	{
		Integer maxAantalFitAanvragenPerRonde = preferenceService.getInteger(PreferenceKey.MAXIMUMAANTALIFOBTS.name());

		return laatsteScreeningRonde.getUitnodigingen().size() >= maxAantalFitAanvragenPerRonde;
	}
}

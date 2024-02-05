package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDate;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseAfspraakDao;
import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsPeriodeDao;
import nl.rivm.screenit.mamma.se.dao.MammaAfsprakenDao;
import nl.rivm.screenit.mamma.se.dto.actions.AfspraakMakenPassantDto;
import nl.rivm.screenit.mamma.se.dto.actions.AfspraakSignalerenDto;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.PassantInschrijvenValidatorService;
import nl.rivm.screenit.mamma.se.service.PassantValidatorResult;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.dashboard.DashboardType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaAfspraakServiceImpl implements MammaAfspraakService
{

	@Autowired
	private MammaAfsprakenDao afsprakenDao;

	@Autowired
	private MammaBaseAfspraakDao baseAfspraakDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaBaseAfspraakService baseAfspraakService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Autowired
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	@Autowired
	private MammaBaseStandplaatsPeriodeDao baseStandplaatsPeriodeDao;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private PassantInschrijvenValidatorService passantInschrijvenValidatorService;

	@Autowired
	private LogService logService;

	@Autowired
	private DashboardService dashboardService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private MailService mailService;

	@Override
	public void setAfspraakStatus(AfspraakSignalerenDto actionDto, MammaAfspraakStatus nieuweStatus, InstellingGebruiker gebruiker)
	{
		final var afspraak = getOfMaakLaatsteAfspraakVanVandaag(actionDto.getAfspraakId(), gebruiker);
		afspraak.setStatus(nieuweStatus);
		hibernateService.saveOrUpdate(afspraak);
	}

	@Override
	public Map<Long, Integer> getIngeschrevenByGebruikerOpDatumVoorSe(Date beginDatum, String seCode)
	{
		var eindDatum = DateUtil.eindDag(beginDatum);
		return afsprakenDao.readInschrijvingenVanSeInRange(beginDatum, eindDatum, seCode);
	}

	@Override
	public LocalDate getDatumVanOudsteNietAfgeslotenOnderzoek(String seCode)
	{
		var datum = baseAfspraakDao.readDatumVanOudsteNietAfgeslotenOnderzoek(currentDateSupplier.getLocalDate(), seCode);
		return DateUtil.toLocalDate(datum);
	}

	@Override
	public void afspraakMakenPassant(AfspraakMakenPassantDto actionDto, InstellingGebruiker gebruiker, MammaScreeningsEenheid screeningsEenheid)
	{
		var client = clientService.getClientByBsn(actionDto.getBsn());
		if (client != null && DateUtil.isGeboortedatumGelijk(actionDto.getGeboortedatum(), client))
		{
			PassantValidatorResult validatorResult = passantInschrijvenValidatorService.isGeldigPassantScenario(client, currentDateSupplier.getLocalDate(), screeningsEenheid);
			if (validatorResult == PassantValidatorResult.OK)
			{
				var laatsteUitnodiging = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging();
				heraanmeldenIndienNodig(gebruiker, client);
				maakAfspraak(gebruiker, screeningsEenheid, client, laatsteUitnodiging);
			}
			else if (validatorResult == PassantValidatorResult.ONGELDIG_ZELFDE_DAG)
			{
				LOG.info("Client met id {} heeft al een afspraak op zelfde dag, tweede aanmelding wordt genegeerd", client.getId());
			}
			else
			{
				throw new IllegalStateException("Er kan geen afspraak gemaakt worden voor de passant");
			}
		}
		else
		{
			throw new IllegalStateException("Er kan geen afspraak gemaakt worden voor de passant");
		}
	}

	@Override
	public MammaAfspraak getOfMaakLaatsteAfspraakVanVandaag(Long afspraakId, InstellingGebruiker gebruiker)
	{
		var afspraak = hibernateService.get(MammaAfspraak.class, afspraakId);
		if (afspraak == null)
		{
			throw new IllegalStateException("Afspraak met afspraakId " + afspraakId + " bestaat niet.");
		}
		var laatsteAfspraak = afspraak.getUitnodiging().getScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak();

		if (!afspraak.equals(laatsteAfspraak))
		{

			if (DateUtil.toLocalDate(laatsteAfspraak.getVanaf()).compareTo(DateUtil.toLocalDate(afspraak.getVanaf())) <= 0)
			{

				return laatsteAfspraak;
			}
			else
			{

				var client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
				heraanmeldenIndienNodig(gebruiker, client);
				var nieuweAfspraak = maakAfspraak(gebruiker, afspraak.getStandplaatsPeriode().getScreeningsEenheid(), client, laatsteAfspraak.getUitnodiging());

				logService.logGebeurtenis(LogGebeurtenis.MAMMA_AFSPRAAK_AFGEMELD_SE_OFFLINE, laatsteAfspraak.getStandplaatsPeriode().getScreeningsEenheid(),
					Collections.singletonList(laatsteAfspraak.getStandplaatsPeriode().getScreeningsEenheid().getBeoordelingsEenheid().getParent().getRegio()), client,
					"Laatste afspraak in de toekomst (" + DateUtil.formatShortDate(laatsteAfspraak.getVanaf()) + ") was afgemeld");

				verstuurMailAfspraakAfgezegdOfflineWerken(laatsteAfspraak);

				return nieuweAfspraak;
			}
		}
		return afspraak;
	}

	private void verstuurMailAfspraakAfgezegdOfflineWerken(MammaAfspraak laatsteAfspraak)
	{
		var dashboardStatussen = dashboardService.getDashboardStatussen(DashboardType.MAMMA_SE_BERICHTEN);
		var dashboardStatus = dashboardStatussen.stream().filter(dashboardStatus1 -> dashboardStatus1.getOrganisatie()
			.equals(laatsteAfspraak.getStandplaatsPeriode().getScreeningsEenheid().getBeoordelingsEenheid().getParent().getRegio())).findFirst().orElseGet(null);

		if (dashboardStatus != null)
		{
			var emailadressen = simplePreferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name());
			if (StringUtils.isNotBlank(dashboardStatus.getEmailadressen()))
			{
				emailadressen = dashboardStatus.getEmailadressen();
			}

			var type = dashboardStatus.getType();
			var content = "Voor een client is een afspraak in de toekomst afgezegd, deze is terug te vinden op het dashboard '" + type.getNaam() + "' van "
				+ dashboardStatus.getOrganisatie().getNaam() + " of via 'Logging inzien' door te filteren op de gebeurtenis 'Afspraak afgemeld SE offline'";

			mailService.queueMailAanProfessional(emailadressen, "Onderzoek nabewerken: afspraak in de toekomst afgezegd", content);
		}
	}

	private MammaAfspraak maakAfspraak(InstellingGebruiker gebruiker, MammaScreeningsEenheid screeningsEenheid, Client client, MammaUitnodiging laatsteUitnodiging)
	{
		var nu = currentDateSupplier.getDate();
		var standplaatsPeriode = baseStandplaatsPeriodeDao.getStandplaatsPeriode(screeningsEenheid, nu);

		if (standplaatsPeriode == null)
		{
			throw new IllegalStateException("Er is geen standplaatsperiode beschikbaar op " + nu.toString() + " voor SE: " + screeningsEenheid.getNaam());
		}

		var vorigeAfspraak = laatsteUitnodiging.getLaatsteAfspraak();
		var annuleerVorigeAfspraak = vorigeAfspraak != null && vorigeAfspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND)
			&& vorigeAfspraak.getVanaf().compareTo(currentDateSupplier.getDate()) > 0;

		baseKansberekeningService.resetPreferences();
		return baseAfspraakService.maakAfspraak(laatsteUitnodiging.getScreeningRonde(),
			baseCapaciteitsBlokService.getCapaciteitsBlokOpTijdstipVoorSe(client, screeningsEenheid, nu), nu,
			standplaatsPeriode, MammaVerzettenReden.PASSANT, annuleerVorigeAfspraak, true, false, true, true, gebruiker, false);
	}

	private void heraanmeldenIndienNodig(InstellingGebruiker gebruiker, Client client)
	{
		if (!client.getMammaDossier().getLaatsteScreeningRonde().getAangemeld())
		{
			baseAfmeldService.heraanmelden(client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteAfmelding(), gebruiker);
		}
	}

}

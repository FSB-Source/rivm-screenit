package nl.rivm.screenit.service.mamma.impl;

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

import java.util.Comparator;
import java.util.Objects;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.exceptions.MammaStandplaatsVanPostcodeOnbekendException;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseClientContactService;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaAfmeldService;
import nl.rivm.screenit.service.mamma.MammaAfspraakReserveringService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseDossierServiceImpl implements MammaBaseDossierService
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	@Lazy
	private ClientService clientService;

	@Autowired
	private BaseDossierService baseDossierService;

	@Autowired
	private BaseClientContactService baseClientContactService;

	@Autowired
	@Lazy
	private MammaBaseScreeningrondeService screeningrondeService;

	@Autowired
	private MammaBaseFollowUpService followUpService;

	@Autowired(required = false)
	@Lazy
	private MammaAfmeldService afmeldService;

	@Autowired
	private MammaBaseOnderzoekService onderzoekService;

	@Autowired
	private MammaBaseStandplaatsService standplaatsService;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Autowired
	private ClientDoelgroepService doelgroepService;

	@Autowired
	private MammaVolgendeUitnodigingService volgendeUitnodigingService;

	@Autowired
	private MammaBaseOnderzoekService baseOnderzoekService;

	@Autowired
	private DashboardService dashboardService;

	@Autowired
	private MammaAfspraakReserveringService afspraakReserveringService;

	@Autowired
	private LogService logService;

	@Override
	public MammaFactorType getFactorType(MammaDossier dossier)
	{
		return MammaFactorType.getFactorType(dossier.getTehuis() != null, dossier.getDoelgroep(), dossier.getEersteOnderzoek());
	}

	@Override
	public long aantalOproepen(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().size();
	}

	@Override
	public long aantalOpgekomenSE(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().stream()
			.filter(sr -> heeftOnderzoek(sr) && !sr.getId().equals(dossier.getLaatsteScreeningRonde().getId())).count();
	}

	@Override
	public long aantalOpgekomenBE(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().stream().filter(this::heeftOnderzoek).count();
	}

	@Override
	public Stream<MammaScreeningRonde> laatste3AfgerondeRondesMetOnderzoek(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().stream().filter(r -> r.getStatus() == ScreeningRondeStatus.AFGEROND).filter(this::heeftOnderzoek)
			.sorted((r1, r2) -> r2.getCreatieDatum().compareTo(r1.getCreatieDatum())).limit(3);
	}

	private boolean heeftOnderzoek(MammaScreeningRonde ronde)
	{
		return ronde.getLaatsteOnderzoek() != null;
	}

	@Override
	public boolean isAfspraakMakenMogelijk(MammaDossier dossier, boolean viaClientportaal, boolean viaSePassant)
	{
		if (heeftGbaPostcode(dossier) && clientService.isLevendeInwonerNederlandMetGbaIndicatie(dossier.getClient()))
		{
			var ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null && !ronde.getMinderValideOnderzoekZiekenhuis()
				&& (ronde.getStatus() == ScreeningRondeStatus.LOPEND || viaSePassant && afmeldService.magEenmaligHeraanmelden(dossier.getClient())))
			{
				var laatsteUitnodiging = ronde.getLaatsteUitnodiging();
				if (laatsteUitnodiging != null)
				{
					var laatsteAfspraak = laatsteUitnodiging.getLaatsteAfspraak();
					MammaOnderzoek onderzoekVanLaatsteAfspraak = null;
					if (laatsteAfspraak != null)
					{
						onderzoekVanLaatsteAfspraak = laatsteAfspraak.getOnderzoek();
					}
					var isOnderzoekVanLaatsteAfspraakOnderbrokenOfOpgeschort = onderzoekVanLaatsteAfspraak != null && onderzoekVanLaatsteAfspraak.isDoorgevoerd()
						&& (onderzoekVanLaatsteAfspraak.getStatus() == MammaOnderzoekStatus.ONDERBROKEN
						|| onderzoekVanLaatsteAfspraak.getStatus() == MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG
						|| MammaBeoordelingStatus.OPGESCHORT.equals(onderzoekVanLaatsteAfspraak.getLaatsteBeoordeling().getStatus()) && !viaClientportaal);

					var isLaatsteAfspraakNoShow = laatsteAfspraak != null && laatsteAfspraak.getStatus() == MammaAfspraakStatus.GEPLAND
						&& laatsteAfspraak.getVanaf().compareTo(currentDateSupplier.getDate()) < 0;

					return !isOnvolledigOnderzoek(ronde.getLaatsteOnderzoek()) &&
						(laatsteAfspraak == null
							|| MammaAfspraakStatus.isGeannuleerd(laatsteAfspraak.getStatus())
							|| isLaatsteAfspraakNoShow
							|| isOnderzoekVanLaatsteAfspraakOnderbrokenOfOpgeschort);
				}
			}
		}
		return false;
	}

	private boolean isOnvolledigOnderzoek(MammaOnderzoek onderzoek)
	{
		return onderzoek != null && onderzoek.isDoorgevoerd() && onderzoek.getStatus().equals(MammaOnderzoekStatus.ONVOLLEDIG);
	}

	@Override
	public boolean isVerzettenMogelijk(MammaDossier dossier)
	{
		if (heeftGbaPostcode(dossier) && clientService.isLevendeInwonerNederlandMetGbaIndicatie(dossier.getClient()))
		{
			var ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null && ronde.getStatus() == ScreeningRondeStatus.LOPEND)
			{
				var laatsteUitnodiging = ronde.getLaatsteUitnodiging();
				if (laatsteUitnodiging != null)
				{
					var laatsteAfspraak = laatsteUitnodiging.getLaatsteAfspraak();
					if (laatsteAfspraak != null && !isAfspraakMakenMogelijk(dossier, false, false))
					{
						return laatsteAfspraak.getStatus() == MammaAfspraakStatus.GEPLAND && !isOnvolledigOnderzoek(ronde.getLaatsteOnderzoek());
					}
				}
			}
		}
		return false;
	}

	@Override
	public MammaOnderzoek getLaatsteOnderzoek(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().stream()
			.flatMap(screeningRonde -> screeningRonde.getUitnodigingen().stream())
			.flatMap(uitnodiging -> uitnodiging.getAfspraken().stream())
			.map(MammaAfspraak::getOnderzoek)
			.filter(Objects::nonNull).max(Comparator.comparing(MammaOnderzoek::getCreatieDatum)).orElse(null);
	}

	@Override
	public boolean isAfspraakForcerenMogelijk(MammaDossier dossier)
	{
		var magAfspraakForceren = false;
		if (dossier.getLaatsteScreeningRonde() != null)
		{
			var onderzoek = dossier.getLaatsteScreeningRonde().getLaatsteOnderzoek();

			if (onderzoek != null && onderzoek.isDoorgevoerd() && onderzoek.getMammografie() != null)
			{
				int maxDagenUitstellenForceren = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_UITNODIGINGEN.name());
				var afspraakDatum180Dagen = DateUtil.toLocalDate(onderzoek.getMammografie().getAfgerondOp()).plusDays(maxDagenUitstellenForceren);
				magAfspraakForceren = currentDateSupplier.getLocalDate().isBefore(afspraakDatum180Dagen);
			}
		}
		return magAfspraakForceren;
	}

	@Override
	public boolean isRondeForcerenMogelijk(MammaDossier dossier)
	{
		if (dossier.getStatus() != DossierStatus.ACTIEF
			|| dossier.getTehuis() != null
			|| !heeftGbaPostcode(dossier)
			|| dossier.getDeelnamemodus() == Deelnamemodus.SELECTIEBLOKKADE)
		{
			return false;
		}
		var laatsteRonde = dossier.getLaatsteScreeningRonde();
		if (laatsteRonde == null)
		{
			return true;
		}
		var laatsteOnderzoek = laatsteRonde.getLaatsteOnderzoek();
		if (laatsteRonde.getStatus() == ScreeningRondeStatus.LOPEND)
		{
			if (laatsteRonde.getLaatsteUitnodiging() == null)
			{
				return true;
			}
			else if (laatsteOnderzoek == null)
			{
				return false;
			}
		}

		if (laatsteRonde.getStatus() == ScreeningRondeStatus.AFGEROND && afmeldService.magEenmaligHeraanmelden(dossier.getClient()))
		{
			return false;
		}

		if (onderzoekWachtNogOpUitslag(laatsteOnderzoek))
		{
			return false;
		}
		return onderzoekService.heeftBinnenMammografieIntervalGeenOnderzoekGehad(dossier);
	}

	private boolean onderzoekWachtNogOpUitslag(MammaOnderzoek laatsteOnderzoek)
	{
		if (laatsteOnderzoek != null)
		{
			var onderzoekHeeftEenUitslag = laatsteOnderzoek.getLaatsteBeoordeling() != null
				&& MammaBeoordelingStatus.isUitslagStatus(laatsteOnderzoek.getLaatsteBeoordeling().getStatus());

			return !onderzoekService.isOnderzoekOnvolledigZonderFotos(laatsteOnderzoek) && !onderzoekHeeftEenUitslag
				&& laatsteOnderzoek.getStatus() != MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG;
		}
		return false;
	}

	private boolean heeftGbaPostcode(MammaDossier dossier)
	{
		return StringUtils.isNotBlank(clientService.getGbaPostcode(dossier.getClient()));
	}

	@Override
	public void rondeForceren(Client client) throws MammaStandplaatsVanPostcodeOnbekendException
	{
		var standplaats = standplaatsService.getStandplaatsMetPostcode(client);
		if (standplaats == null)
		{
			LOG.info("Client {} niet te koppelen aan standplaats", client.getId());
			throw new MammaStandplaatsVanPostcodeOnbekendException();
		}
		var standplaatsPeriode = standplaatsService.huidigeStandplaatsPeriodeInRouteVanStandplaats(standplaats);
		if (standplaatsPeriode == null)
		{
			LOG.info("Standplaats {} niet in route", standplaats.getId());
			throw new MammaStandplaatsVanPostcodeOnbekendException();
		}
		var ronde = baseFactory.maakRonde(client.getMammaDossier(), standplaatsPeriode.getStandplaatsRonde(), true);

		baseFactory.maakUitnodiging(ronde, ronde.getStandplaatsRonde(),
			screeningrondeService.bepaalBriefTypeVoorOpenUitnodiging(volgendeUitnodigingService.isSuspect(client.getMammaDossier()),
				client.getMammaDossier().getDoelgroep()));
	}

	@Override
	public boolean isAutomatischRondeForcerenNaHeraanmeldenMogelijk(MammaDossier dossier)
	{
		return doelgroepService.behoortTotMammaLeeftijdDoelgroep(dossier.getClient()) && isRondeForcerenMogelijk(dossier);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void maakDossierLeeg(MammaDossier dossier)
	{
		if (dossier != null)
		{
			var client = dossier.getClient();

			baseClientContactService.verwijderClientContacten(client, Bevolkingsonderzoek.MAMMA);

			screeningrondeService.verwijderAlleScreeningRondes(dossier);

			afspraakReserveringService.verwijderReserveringenVoorClient(client);

			opruimenDossier(dossier);

			var screeningRondeEvent = dossier.getScreeningRondeEvent();
			if (screeningRondeEvent != null)
			{
				dossier.setScreeningRondeEvent(null);
				hibernateService.delete(screeningRondeEvent);
			}

			hibernateService.saveOrUpdate(dossier);
			followUpService.refreshUpdateFollowUpConclusie(dossier);

			baseDossierService.verwijderNietLaatsteDefinitieveAfmeldingenUitDossier(client.getMammaDossier());

			hibernateService.saveOrUpdate(client);

			var projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
			if (projectClient != null)
			{
				clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.VERWIJDERING_VAN_DOSSIER, Bevolkingsonderzoek.MAMMA);
			}
		}
	}

	private void opruimenDossier(MammaDossier dossier)
	{
		dossier.setInactiefVanaf(null);
		dossier.setInactiefTotMet(null);

		if (DossierStatus.INACTIEF.equals(dossier.getStatus()) && Boolean.TRUE.equals(dossier.getAangemeld()))
		{
			dossier.setStatus(DossierStatus.ACTIEF);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean setUitslagenGecontroleerdEnUpdateDashboard(LogRegel logRegel, InstellingGebruiker medewerker, DashboardStatus dashboardStatus)
	{
		var dossier = logRegel.getClient().getMammaDossier();

		var laatsteOnderzoekZonderUitslag = baseOnderzoekService.getLaatsteOnderzoekMetMissendeUitslagVanDossier(dossier);
		var status = dashboardService.updateLogRegelMetDashboardStatus(logRegel, medewerker.getMedewerker().getGebruikersnaam(), dashboardStatus);

		logService.logGebeurtenis(LogGebeurtenis.MAMMA_CONTROLE_MISSENDE_UITSLAGEN_MATCH_GECONTROLEERD, medewerker, dossier.getClient(), Bevolkingsonderzoek.MAMMA);

		if (laatsteOnderzoekZonderUitslag.isEmpty())
		{
			LOG.warn("Er is geen onderzoek zonder uitslag gevonden voor dossier {}", dossier.getId());
		}
		else
		{
			dossier.setDatumLaatstGecontroleerdeSignalering(laatsteOnderzoekZonderUitslag.get().getAfgerondOp());
			hibernateService.saveOrUpdate(dossier);
		}
		return status;
	}
}

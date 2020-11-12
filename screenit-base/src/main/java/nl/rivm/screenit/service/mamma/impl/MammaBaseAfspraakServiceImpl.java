package nl.rivm.screenit.service.mamma.impl;

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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseAfspraakDao;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKandidaatAfsprakenDeterminatiePeriode;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseAfspraakServiceImpl implements MammaBaseAfspraakService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseAfspraakServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseAfspraakDao afspraakDao;

	@Autowired
	private MammaBaseStandplaatsService standplaatsService;

	@Autowired
	private MammaBaseUitstelService uitstelService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private MammaBaseDossierService dossierService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private LogService logService;

	@Autowired
	private BerichtToSeRestBkService berichtToSeRestBkService;

	@Autowired
	private MammaBaseKansberekeningService kansberekeningService;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService baseBriefService;

	@Override
	public List<MammaKandidaatAfspraakDto> getKandidaatAfspraken(Client client, IMammaAfspraakWijzigenFilter filter)
	{
		List<MammaKandidaatAfspraakDto> kandidaatAfspraakDtos = new ArrayList<>();
		LocalTime nu = dateSupplier.getLocalTime();

		MammaDossier dossier = client.getMammaDossier();

		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_CAPACITEIT_VOLLEDIG_BENUT_TOT_EN_MET_AANTAL_WERKDAGEN.toString());
		Integer minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());

		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = standplaatsService.getStandplaatsPeriodeMetAfstandDtos(client, filter);
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			MammaUitnodiging laatsteUitnodiging = dossier.getLaatsteScreeningRonde() != null ? dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() : null;

			BigDecimal voorlopigeOpkomstkans = laatsteUitnodiging != null
				? kansberekeningService.getVoorlopigeOpkomstkans(laatsteUitnodiging, standplaatsPeriode, filter.getVerzettenReden())
				: kansberekeningService.getVoorlopigeOpkomstkans(dossier, standplaatsPeriode, filter.getVerzettenReden(), BriefType.MAMMA_AFSPRAAK_UITNODIGING);

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null)
			{
				LocalDate standplaatsPeriodeVanaf = DateUtil.toLocalDate(standplaatsPeriode.getVanaf());
				LocalDate standplaatsPeriodeTotEnMet = DateUtil.toLocalDate(standplaatsPeriode.getTotEnMet());

				LocalDate vanafDatum = Collections.max(Arrays.asList(new LocalDate[] { filter.getVanafLocalDate(), standplaatsPeriodeVanaf }));
				LocalDate totEnMetDatum = Collections.min(Arrays.asList(new LocalDate[] { filter.getTotEnMetLocalDate(), vrijgegevenTotEnMetDatum, standplaatsPeriodeTotEnMet }));

				MammaBaseKandidaatAfsprakenDeterminatiePeriode baseKandidaatAfsprakenDeterminatiePeriode = SpringBeanProvider.getInstance()
					.getBean(MammaBaseKandidaatAfsprakenDeterminatiePeriode.class);

				List<MammaKandidaatAfspraak> kandidaatAfsprakenStandplaatsPeriode = baseKandidaatAfsprakenDeterminatiePeriode.getKandidaatAfspraken(dossier, standplaatsPeriode,
					vroegstMogelijkeUitnodigingsDatum(dossier, vanafDatum, minimaleIntervalMammografieOnderzoeken), totEnMetDatum, filter.getExtraOpties(), voorlopigeOpkomstkans,
					capaciteitVolledigBenutTotEnMetAantalWerkdagen, true);
				kandidaatAfsprakenStandplaatsPeriode.forEach(kandidaatAfspraak -> {
					if (kandidaatAfspraak.getDatum().isAfter(dateSupplier.getLocalDate()) || !kandidaatAfspraak.getVanaf().isBefore(nu))
					{
						kandidaatAfspraakDtos
							.add(new MammaKandidaatAfspraakDto(kandidaatAfspraak.getCapaciteitBlokDto().id, kandidaatAfspraak.getDatum(), kandidaatAfspraak.getVanaf(),
								standplaatsPeriode.getId(), standplaatsPeriodeMetAfstandDto.getAfstand()));
					}
				});
			}
		}
		return kandidaatAfspraakDtos;
	}

	@Override
	public boolean valideUitstelStreefDatum(LocalDate streefDatum, MammaStandplaatsPeriode standplaatsPeriode)
	{
		LocalDate standplaatsPeriodeVanaf = DateUtil.toLocalDate(standplaatsPeriode.getVanaf());
		LocalDate standplaatsPeriodeTotEnMet = DateUtil.toLocalDate(standplaatsPeriode.getTotEnMet());
		if (!standplaatsPeriodeVanaf.isAfter(streefDatum) && !standplaatsPeriodeTotEnMet.isBefore(streefDatum))
		{
			LocalDate vrijgegevenTotEnMet = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			return vrijgegevenTotEnMet != null && streefDatum.isAfter(vrijgegevenTotEnMet);
		}
		return false;
	}

	@Override
	public LocalDate vroegstMogelijkeUitnodigingsDatum(MammaDossier dossier, LocalDate voorstelDatum, Integer minimaleIntervalMammografieOnderzoeken)
	{
		Date laatsteMammografieAfgerond = dossier.getLaatsteMammografieAfgerond();
		MammaOnderzoek onderzoek = dossierService.getLaatsteOnderzoek(dossier);
		boolean heeftGeforceerdeAfspraak = dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getAfspraken().stream().anyMatch(MammaAfspraak::isGeforceerdeAfspraak);
		if (laatsteMammografieAfgerond != null && onderzoek != null)
		{
			if (onderzoek.getStatus() != MammaOnderzoekStatus.ONDERBROKEN && onderzoek.getStatus() != MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG
				&& !dossierService.isAfspraakForcerenMogelijk(dossier)
				&& !heeftGeforceerdeAfspraak)
			{
				LocalDate minimaalIntervalOnderzoeken = DateUtil.toLocalDate(laatsteMammografieAfgerond).plusDays(minimaleIntervalMammografieOnderzoeken);
				if (minimaalIntervalOnderzoeken.isAfter(voorstelDatum))
				{
					return minimaalIntervalOnderzoeken;
				}
			}
		}
		return voorstelDatum;
	}

	@Override
	public LocalDate laatstMogelijkeUitnodigingsDatum(MammaDossier dossier)
	{
		if (dossier.getLaatsteScreeningRonde() != null)
		{
			MammaOnderzoek laatsteOnderzoek = dossier.getLaatsteScreeningRonde().getLaatsteOnderzoek();
			if (laatsteOnderzoek != null && laatsteOnderzoek.getStatus() == MammaOnderzoekStatus.ONDERBROKEN)
			{
				MammaMammografie mammografieVanLaatsteAfspraak = laatsteOnderzoek.getMammografie();
				if (MammaMammografieIlmStatus.beeldenBeschikbaarOfBeschikbaarGeweest(mammografieVanLaatsteAfspraak.getIlmStatus()))
				{
					return DateUtil.toLocalDate(mammografieVanLaatsteAfspraak.getAfgerondOp()).plusMonths(6);
				}
			}
		}
		return null;
	}

	@Override
	public List<MammaAfspraak> getAfspraken(MammaScreeningsEenheid screeningsEenheid, Date vanaf, Date totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		List<MammaAfspraak> afspraken = afspraakDao.getAfspraken(screeningsEenheid, vanaf, totEnMet, afspraakStatussen);
		bepaalBenodigdeCapaciteit(afspraken, screeningsEenheid);
		return afspraken;
	}

	@Override
	public void bepaalBenodigdeCapaciteit(List<MammaAfspraak> afspraken, MammaScreeningsEenheid screeningsEenheid)
	{
		ScreeningOrganisatie screeningOrganisatie = (ScreeningOrganisatie) HibernateHelper.deproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());

		for (MammaAfspraak afspraak : afspraken)
		{
			if (afspraak.getBenodigdeCapaciteit() == null) 
			{
				MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
				BigDecimal factor = dossierService.getFactorType(dossier).getFactor(screeningOrganisatie);
				BigDecimal opkomstkans = afspraak.getOpkomstkans().getOpkomstkans();
				afspraak.setBenodigdeCapaciteit(factor.multiply(opkomstkans));
			}
		}
	}

	@Transactional(propagation = Propagation.MANDATORY)
	@Override
	public MammaAfspraak maakAfspraak(MammaScreeningRonde screeningRonde, MammaCapaciteitBlok capaciteitBlok, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden, boolean vorigeAfspraakVerzetten, boolean notificeerBetrokkenSe, boolean isBulk, boolean stuurBerichtNaarSectra, boolean logGebeurtenis,
		Account account, boolean isGeforceerdeAfspraak)
	{
		MammaUitnodiging laatsteUitnodiging = screeningRonde.getLaatsteUitnodiging();

		Set<MammaScreeningsEenheid> genotificeerdeSes = new HashSet<>();
		MammaAfspraak laatsteAfspraak = laatsteUitnodiging.getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			genotificeerdeSes = afspraakAnnuleren(laatsteAfspraak, MammaAfspraakStatus.VERPLAATST, null, vorigeAfspraakVerzetten);
		}

		if (screeningRonde.getLaatsteUitstel() != null)
		{
			uitstelService.uitstelAfzeggen(screeningRonde.getLaatsteUitstel(), MammaUitstelGeannuleerdReden.NIEUWE_AFSPRAAK, dateSupplier.getDate());
		}

		MammaAfspraak afspraak = baseFactory.maakAfspraak(screeningRonde, capaciteitBlok, vanaf, standplaatsPeriode, verzettenReden, notificeerBetrokkenSe, genotificeerdeSes,
			stuurBerichtNaarSectra, isGeforceerdeAfspraak);

		if (logGebeurtenis)
		{
			String melding = getSaveAfspraakMelding(laatsteAfspraak, vanaf, standplaatsPeriode, isBulk, isGeforceerdeAfspraak);
			logService.logGebeurtenis(isGeforceerdeAfspraak ? LogGebeurtenis.MAMMA_AFSPRAAK_GEFORCEERD : LogGebeurtenis.MAMMA_AFSPRAAK_VERZET, account,
				screeningRonde.getDossier().getClient(), melding, Bevolkingsonderzoek.MAMMA);
		}

		kansberekeningService.dossierEventHerzien(screeningRonde.getDossier());

		return afspraak;
	}

	private String getSaveAfspraakMelding(MammaAfspraak huidigeAfspraak, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode, boolean isBulk, boolean isGeforceerdeAfspraak)
	{
		String melding;
		if (huidigeAfspraak == null)
		{
			melding = String.format("Aangemaakt op %1$s in %2$s met %3$s", Constants.getDateTimeFormat().format(vanaf),
				standplaatsPeriode.getStandplaatsRonde().getStandplaats().getNaam(),
				standplaatsPeriode.getScreeningsEenheid().getNaam());
		}
		else
		{
			melding = String.format("Verzet van %1$s in %2$s naar %3$s in %4$s met %5$s", Constants.getDateTimeFormat().format(huidigeAfspraak.getVanaf()),
				huidigeAfspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getNaam(), Constants.getDateTimeFormat().format(vanaf),
				standplaatsPeriode.getStandplaatsRonde().getStandplaats().getNaam(),
				standplaatsPeriode.getScreeningsEenheid().getNaam());
			if (isBulk)
			{
				melding += " (bulk)";
			}
		}
		if (isGeforceerdeAfspraak)
		{
			melding += " (geforceerd)";
		}
		return melding;
	}

	@Override
	public BigDecimal getBenodigdeCapaciteit(List<MammaAfspraak> afspraken)
	{
		return afspraken.stream().map(MammaAfspraak::getBenodigdeCapaciteit).reduce(BigDecimal.ZERO, BigDecimal::add);
	}

	@Override
	public long countAfspraken(long standplaatsPeriodeId, MammaAfspraakStatus... afspraakStatussen)
	{
		return afspraakDao.countAfspraken(standplaatsPeriodeId, afspraakStatussen);
	}

	@Override
	public long countAfspraken(MammaScreeningsEenheid screeningsEenheid, Date vanaf, Date totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return afspraakDao.countAfspraken(screeningsEenheid, vanaf, totEnMet, afspraakStatussen);
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public int koppelNietGekoppeldeAfspraken(MammaCapaciteitBlok capaciteitsBlok, boolean runDry)
	{
		int aantalAfspraken = 0;
		if (capaciteitsBlok.getBlokType() != MammaCapaciteitBlokType.GEEN_SCREENING)
		{
			LOG.debug("Zoek afspraken voor cap.blok om te kunnen (her)koppelen");
			List<MammaAfspraak> afspraken = afspraakDao.getNietGekoppeldeAfspraken(capaciteitsBlok);
			aantalAfspraken = afspraken.size();
			if (!runDry)
			{
				for (MammaAfspraak afspraak : afspraken)
				{
					LOG.info("Afspraak van " + Constants.getDateTimeFormat().format(afspraak.getVanaf()) + " + voor client met id "
						+ afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient().getId() + " gekoppeld aan cap.blok");
					capaciteitsBlok.getAfspraken().add(afspraak);
					afspraak.setCapaciteitBlok(capaciteitsBlok);
					hibernateService.saveOrUpdate(afspraak);
				}
				hibernateService.saveOrUpdate(capaciteitsBlok);
			}
		}
		return aantalAfspraken;
	}

	@Override
	public void afspraakAnnuleren(MammaAfspraak afspraak, MammaAfspraakStatus nieuweStatus, Date rondeAfgemeldOp)
	{
		boolean afspraakStatusWijzigen = afspraak.getVanaf().compareTo(dateSupplier.getDate()) > 0;
		afspraakAnnuleren(afspraak, nieuweStatus, rondeAfgemeldOp, afspraakStatusWijzigen);
	}

	@Override
	public Set<MammaScreeningsEenheid> afspraakAnnuleren(MammaAfspraak afspraak, MammaAfspraakStatus nieuweStatus, Date rondeAfgemeldOp, boolean afspraakStatusWijzigen)
	{
		Set<MammaScreeningsEenheid> genotificeerdeSes = new HashSet<>();

		if (afspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND))
		{

			if (nieuweStatus != null && afspraakStatusWijzigen)
			{
				afspraak.setStatus(nieuweStatus);
				afspraak.setAfgezegdOp(rondeAfgemeldOp);

				MammaCapaciteitBlok capaciteitBlok = afspraak.getCapaciteitBlok();
				if (capaciteitBlok != null)
				{
					capaciteitBlok.getAfspraken().remove(afspraak);
					afspraak.setCapaciteitBlok(null);
					hibernateService.saveOrUpdate(capaciteitBlok);
				}

				MammaScreeningRonde screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
				baseBriefService.setNietGegenereerdeBrievenOpTegenhouden(screeningRonde, BriefType.MAMMA_AFSPRAAK_VERZET);
				hibernateService.saveOrUpdate(afspraak);

				this.berichtToBatchService.queueMammaHL7v24BerichtUitgaand(screeningRonde.getDossier().getClient(), MammaHL7v24ORMBerichtStatus.CANCELLED);

				genotificeerdeSes = berichtToSeRestBkService.notificeerSesEnGeefSesTerug(screeningRonde.getDossier().getClient());
			}
		}
		return genotificeerdeSes;
	}

	@Override
	public MammaStandplaatsLocatie getMammaStandplaatsLocatieAfspraak(MammaAfspraak afspraak)
	{
		MammaStandplaatsLocatie locatie = null;
		if (afspraak != null)
		{
			MammaStandplaats standplaats = afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
			Date datumAfspraak = DateUtil.toUtilDateMidnight(afspraak.getVanaf());
			locatie = standplaatsService.getStandplaatsLocatie(standplaats, datumAfspraak);
		}
		return locatie;
	}

	@Override
	public MammaStandplaatsLocatie getMammaStandplaatsLocatieUitnodiging(MammaUitnodiging uitnodiging)
	{
		MammaStandplaatsLocatie locatie = null;
		if (uitnodiging != null)
		{
			if (uitnodiging.getStandplaatsRonde() != null)
			{
				MammaStandplaats standplaats = uitnodiging.getStandplaatsRonde().getStandplaats();
				locatie = standplaatsService.getStandplaatsLocatie(standplaats, dateSupplier.getDate());
			}
			else if (uitnodiging.getLaatsteAfspraak() != null)
			{
				MammaStandplaats standplaats = uitnodiging.getLaatsteAfspraak().getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
				locatie = standplaatsService.getStandplaatsLocatie(standplaats, dateSupplier.getDate());
			}
		}
		return locatie;
	}

	@Override
	public MammaAfspraak getLaatsteAfspraakVanBriefronde(Brief brief)
	{
		brief = (Brief) HibernateHelper.deproxy(brief);
		if (brief instanceof MammaBrief)
		{
			MammaScreeningRonde screeningRonde = ((MammaBrief) brief).getScreeningRonde();
			if (screeningRonde != null && screeningRonde.getLaatsteUitnodiging() != null)
			{
				return screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
			}
		}
		return null;
	}

	@Override
	public MammaUitnodiging getLaatsteUitnodigingVanScreeningRonde(MammaScreeningRonde ronde)
	{
		if (ronde != null)
		{
			if (ronde.getLaatsteUitnodiging() != null)
			{
				return ronde.getLaatsteUitnodiging();
			}
		}
		return null;
	}

	@Override
	public boolean isAfspraakBinnen180Dagen(MammaOnderzoek onderzoek)
	{
		Integer minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());
		LocalDate minimaalIntervalOnderzoeken = DateUtil.toLocalDate(onderzoek.getMammografie().getAfgerondOp()).plusDays(minimaleIntervalMammografieOnderzoeken);
		return minimaalIntervalOnderzoeken.isAfter(currentDateSupplier.getLocalDate());
	}
}

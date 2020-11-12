package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.mamma.MammaBaseAfspraakDao;
import nl.rivm.screenit.datasource.DataSourceRouter;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingenPerSeDto;
import nl.rivm.screenit.exceptions.DryRunException;
import nl.rivm.screenit.exceptions.OpslaanAfsprakenBuitenStandplaatsPeriodeException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.dao.PlanningReadModelDao;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsRondeIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptOpslaanService;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class PlanningConceptOpslaanServiceImpl implements PlanningConceptOpslaanService
{
	private static final Logger LOG = LoggerFactory.getLogger(PlanningConceptOpslaanServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseAfspraakService baseAfspraakService;

	@Autowired
	private MammaBaseAfspraakDao baseAfspraakDao;

	@Autowired
	private PlanningReadModelDao readModelDao;

	@Override
	@Transactional(rollbackFor = { OpslaanVerwijderenTijdBlokException.class, DryRunException.class, OpslaanAfsprakenBuitenStandplaatsPeriodeException.class })
	public PlanningConceptMeldingenDto opslaan(Long screeningOrganisatieId, boolean runDry)
		throws OpslaanVerwijderenTijdBlokException, DryRunException, OpslaanAfsprakenBuitenStandplaatsPeriodeException
	{
		DataSourceRouter.useReadWrite();
		PlanningConceptMeldingenDto meldingenDto = new PlanningConceptMeldingenDto();

		Map<Long, PlanningBlok> nieuweBlokken = new HashMap<>();
		PlanningScreeningsOrganisatie screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get(screeningOrganisatieId);

		Map<Long, Date[]> afsprakenBuitenStandplaatsPeriodeMap = new HashMap<>();

		se: for (PlanningScreeningsEenheid screeningsEenheid : screeningsOrganisatie.getScreeningsEenheidSet())
		{
			MammaScreeningsEenheid persistentScreeningsEenheid = hibernateService.get(MammaScreeningsEenheid.class, screeningsEenheid.getId());
			LOG.info("Concept opslaan voor SE " + persistentScreeningsEenheid.getNaam());

			PlanningStandplaatsPeriode eersteStandplaatsPeriodeMetPrognose = screeningsEenheid.getStandplaatsPeriodeNavigableSet().stream()
				.filter(PlanningStandplaatsPeriode::getPrognose)
				.findFirst().orElse(null);

			if (eersteStandplaatsPeriodeMetPrognose != null && eersteStandplaatsPeriodeMetPrognose.getId() != null)
			{
				MammaStandplaatsPeriode persistentStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, eersteStandplaatsPeriodeMetPrognose.getId());
				Date conceptTotEnMet = DateUtil.toUtilDate(eersteStandplaatsPeriodeMetPrognose.getTotEnMet().plusDays(1));
				Date persistentTotEnMet = DateUtil.toUtilDate(DateUtil.toLocalDate(persistentStandplaatsPeriode.getTotEnMet()).plusDays(1));
				if (conceptTotEnMet.compareTo(persistentTotEnMet) != 0)
				{
					List<Date> totEnMetDatumList = Arrays.asList(conceptTotEnMet, persistentTotEnMet);
					Date[] eersteEnLaatsteAfspraakVanaf = baseAfspraakDao.getEersteEnLaatsteAfspraakDatum(persistentStandplaatsPeriode.getId(),
						Collections.min(totEnMetDatumList), Collections.max(totEnMetDatumList), MammaAfspraakStatus.GEPLAND);

					if (eersteEnLaatsteAfspraakVanaf[0] != null)
					{
						String melding = "Concept kan niet worden opgeslagen voor SE " + persistentScreeningsEenheid.getNaam() + ". Standplaatsperiode "
							+ eersteStandplaatsPeriodeMetPrognose.getId() + " heeft afspraken op " + DateUtil.formatShortDate(eersteEnLaatsteAfspraakVanaf[0]);

						if (eersteEnLaatsteAfspraakVanaf[1] != null)
						{
							melding += " t/m " + DateUtil.formatShortDate(eersteEnLaatsteAfspraakVanaf[1]);
						}

						LOG.warn(melding);
						afsprakenBuitenStandplaatsPeriodeMap.put(persistentScreeningsEenheid.getId(), eersteEnLaatsteAfspraakVanaf);
					}
				}
			}

			if (!afsprakenBuitenStandplaatsPeriodeMap.isEmpty())
			{
				continue se;
			}

			for (PlanningBlok blokToRemove : PlanningBlokIndex.getBlokDeletedSet(screeningsEenheid))
			{
				if (blokToRemove.getId() != null)
				{
					LOG.info("Verwijder cap.blok " + blokToRemove.getCapaciteitBlokType() + " - " + blokToRemove.getVanaf());
					MammaCapaciteitBlok persistentBlok = hibernateService.get(MammaCapaciteitBlok.class, blokToRemove.getId());
					if (persistentBlok != null)
					{
						String melding = "Capaciteit verwijderd (" + Constants.getDateTimeFormat().format(blokToRemove.getDateVanaf()) + ").";
						MammaMeldingNiveau niveau = MammaMeldingNiveau.INFO;
						if (!persistentBlok.getAfspraken().isEmpty())
						{
							melding += ". Gekoppelde afspraken (#" + persistentBlok.getAfspraken().size() + ") worden losgemaakt van deze capaciteitsblok.";
							niveau = MammaMeldingNiveau.WAARSCHUWING;
						}
						if (addMelding(meldingenDto, screeningsEenheid, melding, niveau) && runDry)
						{
							continue se;
						}

						if (!runDry)
						{
							ontkoppelAfspraken(persistentBlok, true);
							hibernateService.delete(persistentBlok);
						}
					}
				}
				if (!runDry)
				{
					screeningsEenheid.getBlokSet().remove(blokToRemove);
				}
			}

			boolean isMaxBereikt = false;
			Set<MammaCapaciteitBlok> ontkoppelAfsprakenCapaciteitBlokken = new HashSet<>();
			Set<MammaCapaciteitBlok> koppelAfsprakenCapaciteitBlokken = new HashSet<>();
			Map<MammaCapaciteitBlok, PlanningMeldingDto> capaciteitBlokMeldingMap = new HashMap<>();

			changedBlokken: for (PlanningBlok blok : PlanningBlokIndex.getBlokChangedSet(screeningsEenheid))
			{
				MammaCapaciteitBlok persistentBlok = null;
				boolean isNieuw = blok.getId() == null;
				LOG.info("Nieuw/wijzig cap.blok " + blok.getCapaciteitBlokType() + " - " + blok.getDateVanaf() + ". Nieuw? " + isNieuw);
				if (!isNieuw)
				{
					persistentBlok = hibernateService.get(MammaCapaciteitBlok.class, blok.getId());
				}
				if (persistentBlok == null)
				{
					persistentBlok = new MammaCapaciteitBlok();
					persistentBlok.setScreeningsEenheid(persistentScreeningsEenheid);
				}

				String waarschuwing = "";
				boolean ontkoppelAfspraken = false;
				int aantalAfsprakenTeVerwijderen = getAantalAfsprakenTeOntkoppelen(persistentBlok, blok.getDateVanaf(), blok.getDateTot(), blok.getCapaciteitBlokType());
				if (aantalAfsprakenTeVerwijderen > 0)
				{
					waarschuwing = " Blok type of tijden zijn gewijzigd. Gekoppelde afspraken (#" + aantalAfsprakenTeVerwijderen + ") worden losgemaakt van deze capaciteitsblok. ";
					ontkoppelAfspraken = true;
				}
				persistentBlok.setAantalOnderzoeken(blok.getAantalOnderzoeken());
				persistentBlok.setBlokType(blok.getCapaciteitBlokType());
				persistentBlok.setOpmerkingen(blok.getOpmerkingen());
				persistentBlok.setTot(blok.getDateTot());
				persistentBlok.setVanaf(blok.getDateVanaf());
				persistentBlok.setMinderValideAfspraakMogelijk(blok.isMinderValideAfspraakMogelijk());

				String melding = "";

				if (isNieuw)
				{
					melding += "Capaciteit aangemaakt (" + Constants.getDateTimeFormat().format(blok.getDateVanaf()) + ").";
				}
				else
				{
					String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(persistentBlok, hibernateService.getHibernateSession());
					if (diffToLatestVersion.length() > 0)
					{
						melding += "Capaciteit gewijzigd (" + diffToLatestVersion + ").";
					}
				}
				if (StringUtils.isNotBlank(melding))
				{
					if (!runDry)
					{
						if (ontkoppelAfspraken)
						{

							ontkoppelAfsprakenCapaciteitBlokken.add(persistentBlok);
						}
						hibernateService.saveOrUpdate(persistentBlok);
						koppelAfsprakenCapaciteitBlokken.add(persistentBlok);
						if (isNieuw)
						{
							nieuweBlokken.put(persistentBlok.getId(), blok);
						}
					}
					else
					{
						koppelAfsprakenCapaciteitBlokken.add(persistentBlok);
					}
					MammaMeldingNiveau niveau = MammaMeldingNiveau.INFO;
					if (waarschuwing.length() > 0)
					{
						niveau = MammaMeldingNiveau.WAARSCHUWING;
					}
					isMaxBereikt = addMelding(meldingenDto, screeningsEenheid, melding + waarschuwing, niveau);

					if (isMaxBereikt && runDry)
					{

						break changedBlokken;
					}
					else
					{
						List<PlanningMeldingDto> seMeldingenDtoList = getMeldingenPerSeDto(meldingenDto, screeningsEenheid).meldingen;
						PlanningMeldingDto meldingDto = seMeldingenDtoList.get(seMeldingenDtoList.size() - 1);
						capaciteitBlokMeldingMap.put(persistentBlok, meldingDto);
					}
				}
			}

			ontkoppelAfsprakenCapaciteitBlokken.forEach(ontkoppelAfsprakenCapaciteitBlok -> ontkoppelAfspraken(ontkoppelAfsprakenCapaciteitBlok, false));

			koppelAfsprakenCapaciteitBlokken.forEach(teKoppelenBlok -> {
				String waarschuwing = koppelNietGekoppeldeAfspraken(teKoppelenBlok, runDry);
				if (capaciteitBlokMeldingMap.containsKey(teKoppelenBlok))
				{
					PlanningMeldingDto meldingDto = capaciteitBlokMeldingMap.get(teKoppelenBlok);
					meldingDto.tekst += waarschuwing;
				}

			});

			if (isMaxBereikt && runDry)
			{
				continue se;
			}

			for (PlanningStandplaatsPeriode standplaatsPeriode : screeningsEenheid.getStandplaatsPeriodeNavigableSet())
			{
				String melding = "";
				MammaStandplaatsPeriode persistentStandplaatsPeriode = null;
				MammaStandplaatsRonde persistentStandplaatsRonde = null;
				if (standplaatsPeriode.getId() != null)
				{
					persistentStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, standplaatsPeriode.getId());
					persistentStandplaatsRonde = persistentStandplaatsPeriode.getStandplaatsRonde();
				}
				PlanningStandplaatsRonde standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();
				if (persistentStandplaatsPeriode == null)
				{
					MammaStandplaats persistentStandplaats = hibernateService.get(MammaStandplaats.class, standplaatsRonde.getStandplaats().getId());
					persistentStandplaatsPeriode = new MammaStandplaatsPeriode();
					persistentStandplaatsPeriode.setScreeningsEenheid(persistentScreeningsEenheid);

					persistentStandplaatsRonde = standplaatsRonde.getId() != null
						? hibernateService.get(MammaStandplaatsRonde.class, standplaatsRonde.getId())
						: null;

					if (persistentStandplaatsRonde == null)
					{
						persistentStandplaatsRonde = new MammaStandplaatsRonde();
						persistentStandplaatsRonde.setStandplaats(persistentStandplaats);
						persistentStandplaats.getStandplaatsRonden().add(persistentStandplaatsRonde);

					}
					persistentStandplaatsPeriode.setStandplaatsRonde(persistentStandplaatsRonde);
					persistentStandplaatsRonde.getStandplaatsPerioden().add(persistentStandplaatsPeriode);
					melding = "Nieuw ";
				}

				MammaScreeningsEenheid otherPersistentScreeningsEenheid = persistentStandplaatsPeriode.getScreeningsEenheid();
				if (!otherPersistentScreeningsEenheid.equals(persistentScreeningsEenheid))
				{
					otherPersistentScreeningsEenheid.getStandplaatsPerioden().remove(persistentStandplaatsPeriode);
					persistentStandplaatsPeriode.setScreeningsEenheid(persistentScreeningsEenheid);

					persistentScreeningsEenheid.getStandplaatsPerioden().add(persistentStandplaatsPeriode);
				}
				persistentStandplaatsPeriode.setPrognose(standplaatsPeriode.getPrognose());
				persistentStandplaatsPeriode.setTotEnMet(DateUtil.toUtilDate(standplaatsPeriode.getTotEnMet()));
				persistentStandplaatsPeriode.setVanaf(DateUtil.toUtilDate(standplaatsPeriode.getVanaf()));
				persistentStandplaatsPeriode.setScreeningsEenheidVolgNr(standplaatsPeriode.getScreeningsEenheidVolgNr());
				persistentStandplaatsPeriode.setStandplaatsRondeVolgNr(standplaatsPeriode.getStandplaatsRondeVolgNr());
				String diffStandplaatsPeriodeToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(persistentStandplaatsPeriode, hibernateService.getHibernateSession());

				persistentStandplaatsRonde.setAfspraakDrempel(standplaatsRonde.getAfspraakDrempel());
				persistentStandplaatsRonde.setAchtervangToegepast(standplaatsRonde.getAchtervangToegepast());
				persistentStandplaatsRonde.setMinderValideUitnodigenVanaf(DateUtil.toUtilDate(standplaatsRonde.getMinderValideUitnodigenVanaf()));

				MammaStandplaats achtervangStandplaats = standplaatsRonde.getAchtervangStandplaats() != null
					? hibernateService.get(MammaStandplaats.class, standplaatsRonde.getAchtervangStandplaats().getId())
					: null;
				persistentStandplaatsRonde.setAchtervangStandplaats(achtervangStandplaats);

				MammaStandplaats minderValideUitwijkStandplaats = standplaatsRonde.getMinderValideUitwijkStandplaats() != null
					? hibernateService.get(MammaStandplaats.class, standplaatsRonde.getMinderValideUitwijkStandplaats().getId())
					: null;
				persistentStandplaatsRonde.setMinderValideUitwijkStandplaats(minderValideUitwijkStandplaats);
				changeAfspraakcapaciteitBeschikbaarVoor(persistentStandplaatsRonde, standplaatsRonde.getAfspraakcapaciteitBeschikbaarVoor());

				String diffStandplaatsRondeToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(persistentStandplaatsRonde, hibernateService.getHibernateSession());

				if (diffStandplaatsPeriodeToLatestVersion.length() > 0 || diffStandplaatsRondeToLatestVersion.length() > 0)
				{
					if (melding.isEmpty())
					{
						melding = "Gewijzigd ";
					}
					melding += persistentStandplaatsPeriode.getStandplaatsRonde().getStandplaats().getNaam() + " / " + diffStandplaatsPeriodeToLatestVersion + " / "
						+ diffStandplaatsRondeToLatestVersion;
				}
				if (StringUtils.isNotBlank(melding))
				{
					if (addMelding(meldingenDto, screeningsEenheid, melding, MammaMeldingNiveau.INFO) && runDry)
					{
						continue se;
					}
				}
				if (!runDry)
				{
					persistentStandplaatsRonde.setInterval(standplaatsRonde.getInterval());
					standplaatsRonde.setIntieelInterval(standplaatsRonde.getInterval());
					hibernateService.saveOrUpdateAll(persistentStandplaatsRonde, persistentStandplaatsRonde.getStandplaats());
					hibernateService.saveOrUpdate(persistentStandplaatsPeriode);

					PlanningStandplaatsRondeIndex.put(standplaatsRonde);
					standplaatsRonde.setId(persistentStandplaatsRonde.getId());
					standplaatsPeriode.setId(persistentStandplaatsPeriode.getId());
					readModelDao.addStandplaatsPeriode(standplaatsPeriode);
				}
			}

			persistentScreeningsEenheid.setInterval(screeningsEenheid.getInterval());
			persistentScreeningsEenheid.setHerhalingsWeek(DateUtil.toUtilDate(screeningsEenheid.getHerhalingsWeek().getDatum()));
			String diffScreeningsEenheidToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(persistentScreeningsEenheid, hibernateService.getHibernateSession());
			if (diffScreeningsEenheidToLatestVersion.length() > 0)
			{
				String melding = "Gewijzigd: " + diffScreeningsEenheidToLatestVersion;
				addMelding(meldingenDto, screeningsEenheid, melding, MammaMeldingNiveau.INFO);
			}
			if (!runDry)
			{
				PlanningBlokIndex.reset(screeningsEenheid);
				screeningsEenheid.setInitieelInterval(screeningsEenheid.getInterval());
				screeningsEenheid.setInitieelHerhalingsWeek(screeningsEenheid.getHerhalingsWeek());
				hibernateService.saveOrUpdate(persistentScreeningsEenheid);
			}
		}
		if (afsprakenBuitenStandplaatsPeriodeMap.size() > 0)
		{
			throw new OpslaanAfsprakenBuitenStandplaatsPeriodeException(afsprakenBuitenStandplaatsPeriodeMap);
		}
		if (!runDry && meldingenDto.niveau != MammaMeldingNiveau.PROBLEEM)
		{
			screeningsOrganisatie.restConceptGewijzigdDoor();
			nieuweBlokken.forEach((k, v) -> v.setId(k));
		}
		else
		{
			throw new DryRunException(meldingenDto);
		}
		return meldingenDto;
	}

	private void changeAfspraakcapaciteitBeschikbaarVoor(MammaStandplaatsRonde persistentStandplaatsRonde, List<PlanningScreeningsOrganisatie> afspraakcapaciteitBeschikbaarVoor)
	{
		List<ScreeningOrganisatie> sosToDelete = new ArrayList<>();
		List<ScreeningOrganisatie> persistentAfspraakcapaciteitBeschikbaarVoor = persistentStandplaatsRonde.getAfspraakcapaciteitBeschikbaarVoor();
		for (ScreeningOrganisatie persistentScreeningorganisatie : persistentAfspraakcapaciteitBeschikbaarVoor)
		{
			if (afspraakcapaciteitBeschikbaarVoor.stream().noneMatch(so -> so.getId().equals(persistentScreeningorganisatie.getId())))
			{
				sosToDelete.add(persistentScreeningorganisatie);
			}
		}
		persistentAfspraakcapaciteitBeschikbaarVoor.removeAll(sosToDelete);
		for (PlanningScreeningsOrganisatie screeningorganisatie : afspraakcapaciteitBeschikbaarVoor)
		{
			if (persistentAfspraakcapaciteitBeschikbaarVoor.stream().noneMatch(so -> so.getId().equals(screeningorganisatie.getId())))
			{
				persistentAfspraakcapaciteitBeschikbaarVoor.add(hibernateService.load(ScreeningOrganisatie.class, screeningorganisatie.getId()));
			}
		}
	}

	@Override
	public int getAantalAfsprakenTeOntkoppelen(MammaCapaciteitBlok blok, Date vanaf, Date tot, MammaCapaciteitBlokType nieuweBlokType)
	{
		int aantalAfspraken = 0;
		for (MammaAfspraak afspraak : blok.getAfspraken())
		{
			if (moetOntkoppeldWorden(afspraak, vanaf, tot, nieuweBlokType))
			{
				aantalAfspraken++;
			}
		}
		return aantalAfspraken;
	}

	private boolean moetOntkoppeldWorden(MammaAfspraak afspraak, Date vanaf, Date tot, MammaCapaciteitBlokType nieuweBlokType)
	{
		if (MammaAfspraakStatus.isGeannuleerd(afspraak.getStatus()))
		{
			return true;
		}
		if (vanaf.compareTo(afspraak.getVanaf()) > 0 || tot.compareTo(afspraak.getVanaf()) <= 0)
		{
			return true;
		}
		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		switch (nieuweBlokType)
		{
		case REGULIER:
			if (dossier.getTehuis() != null)
			{
				return true;
			}
			break;
		case TEHUIS:
			if (dossier.getTehuis() == null)
			{
				return true;
			}
			break;
		case GEEN_SCREENING:
			return true;
		default:
			throw new IllegalArgumentException("Onbekend bloktype: " + nieuweBlokType);
		}
		return false;
	}

	private String koppelNietGekoppeldeAfspraken(MammaCapaciteitBlok persistentBlok, boolean runDry)
	{
		String aanvullendeMelding = "";
		int aantalAfspraken = baseAfspraakService.koppelNietGekoppeldeAfspraken(persistentBlok, runDry);
		if (aantalAfspraken > 0)
		{
			aanvullendeMelding += aantalAfspraken + " nog niet gekoppelde afspraken worden gekoppeld aan dit capaciteitsblok.";
		}
		return aanvullendeMelding;
	}

	private void ontkoppelAfspraken(MammaCapaciteitBlok persistentBlok, boolean delete)
	{
		for (MammaAfspraak afspraak : persistentBlok.getAfspraken())
		{
			if (delete || moetOntkoppeldWorden(afspraak, persistentBlok.getVanaf(), persistentBlok.getTot(), persistentBlok.getBlokType()))
			{
				afspraak.setCapaciteitBlok(null);
				hibernateService.saveOrUpdate(afspraak);
				LOG.info("Afspraak van " + afspraak.getVanaf() + " + voor clienId " + afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient().getId()
					+ " ontkoppeld van cap.blok");
			}
		}
		persistentBlok.getAfspraken().clear();
	}

	private boolean addMelding(PlanningConceptMeldingenDto meldingenDto, PlanningScreeningsEenheid screeningsEenheid, String melding, MammaMeldingNiveau niveau)
	{
		boolean maxMeldingVoorSeBereikt = false;
		PlanningMeldingDto meldingDto = new PlanningMeldingDto();
		meldingDto.tekst = melding;
		meldingDto.niveau = niveau;
		Long screeningsEenheidId = screeningsEenheid.getId();
		PlanningMeldingenPerSeDto meldingenPerSeDto = getMeldingenPerSeDto(meldingenDto, screeningsEenheid);
		if (meldingenPerSeDto == null)
		{
			meldingenPerSeDto = new PlanningMeldingenPerSeDto();
			meldingenPerSeDto.niveau = niveau;
			meldingenPerSeDto.screeningsEenheidId = screeningsEenheidId;
			meldingenDto.seMeldingen.put(screeningsEenheidId, meldingenPerSeDto);
		}
		if (meldingenPerSeDto.meldingen.size() >= PlanningConstanten.MAX_MELDINGEN_PER_SE)
		{
			meldingDto = new PlanningMeldingDto();
			meldingDto.tekst = "Het zijn meer dan " + PlanningConstanten.MAX_MELDINGEN_PER_SE + " meldingen voor deze SE. Gestopt met zoeken naar wijzigingen.";
			meldingDto.niveau = MammaMeldingNiveau.WAARSCHUWING;
			maxMeldingVoorSeBereikt = true;
		}
		meldingenPerSeDto.meldingen.add(meldingDto);

		if (meldingenDto.niveau == null || meldingenDto.niveau.ordinal() < niveau.ordinal())
		{
			meldingenDto.niveau = niveau;
		}
		if (meldingenPerSeDto.niveau == null || meldingenPerSeDto.niveau.ordinal() < niveau.ordinal())
		{
			meldingenPerSeDto.niveau = niveau;
		}
		LOG.info(screeningsEenheidId + " " + niveau + ": " + melding);
		return maxMeldingVoorSeBereikt;
	}

	private static PlanningMeldingenPerSeDto getMeldingenPerSeDto(PlanningConceptMeldingenDto meldingenDto, PlanningScreeningsEenheid planningScreeningsEenheid)
	{
		Long screeningsEenheidId = planningScreeningsEenheid.getId();
		return meldingenDto.seMeldingen.get(screeningsEenheidId);
	}
}
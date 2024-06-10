package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.mamma.MammaBaseAfspraakDao;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingenPerSeDto;
import nl.rivm.screenit.exceptions.DryRunException;
import nl.rivm.screenit.exceptions.OpslaanAfsprakenBuitenStandplaatsPeriodeException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.exception.MaxMeldingenVoorSeBereiktException;
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
import nl.rivm.screenit.mamma.planning.service.PlanningConceptmodelService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class PlanningConceptOpslaanServiceImpl implements PlanningConceptOpslaanService
{
	private static final String MAX_AANTAL_MELDINGEN_BEREIKT_MELDING = "Het zijn meer dan " + PlanningConstanten.MAX_MELDINGEN_PER_SE
		+ " meldingen voor deze SE. Gestopt met zoeken naar wijzigingen.";

	private final HibernateService hibernateService;

	private final MammaBaseAfspraakService baseAfspraakService;

	private final MammaBaseAfspraakDao baseAfspraakDao;

	private final PlanningConceptmodelService conceptModelService;

	private final MammaBaseStandplaatsService baseStandplaatsService;

	public PlanningConceptOpslaanServiceImpl(HibernateService hibernateService, MammaBaseAfspraakService baseAfspraakService,
		MammaBaseAfspraakDao baseAfspraakDao, @Lazy PlanningConceptmodelService conceptModelService, ICurrentDateSupplier currentDateSupplier,
		MammaBaseStandplaatsService baseStandplaatsService)
	{
		this.hibernateService = hibernateService;
		this.baseAfspraakService = baseAfspraakService;
		this.baseAfspraakDao = baseAfspraakDao;
		this.conceptModelService = conceptModelService;
		this.baseStandplaatsService = baseStandplaatsService;
	}

	private static PlanningMeldingenPerSeDto getMeldingenPerSeDto(PlanningConceptMeldingenDto meldingenDto, PlanningScreeningsEenheid planningScreeningsEenheid)
	{
		var screeningsEenheidId = planningScreeningsEenheid.getId();
		return meldingenDto.seMeldingen.get(screeningsEenheidId);
	}

	@Transactional
	@Override
	public void slaConceptOpVoorAlleScreeningsOrganisaties()
	{
		PlanningDoorrekenenManager.run();

		for (var screeningsOrganisatie : PlanningScreeningsOrganisatieIndex.getScreeningsOrganisaties())
		{
			try
			{
				slaConceptOpVoorScreeningsOrganisatie(screeningsOrganisatie.getId(), false);
			}
			catch (OpslaanAfsprakenBuitenStandplaatsPeriodeException | DryRunException e)
			{
				LOG.error("Niet mogelijk om het concept op te slaan voor so {}", screeningsOrganisatie.getId(), e);
				throw new IllegalStateException(e);
			}
		}
	}

	@Override
	@Transactional(rollbackFor = { OpslaanVerwijderenTijdBlokException.class, DryRunException.class,
		OpslaanAfsprakenBuitenStandplaatsPeriodeException.class }, propagation = Propagation.REQUIRED)
	public PlanningConceptMeldingenDto slaConceptOpVoorScreeningsOrganisatie(Long screeningOrganisatieId, boolean runDry)
		throws DryRunException, OpslaanAfsprakenBuitenStandplaatsPeriodeException
	{
		var meldingenDto = new PlanningConceptMeldingenDto();
		var nieuweBlokken = new HashMap<Long, PlanningBlok>();
		var screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get(screeningOrganisatieId);

		var afsprakenBuitenStandplaatsPeriodeMap = new HashMap<Long, Date[]>();

		for (var screeningsEenheid : screeningsOrganisatie.getScreeningsEenheidSet())
		{
			var persistentScreeningsEenheid = hibernateService.get(MammaScreeningsEenheid.class, screeningsEenheid.getId());
			LOG.info("Concept opslaan voor SE {}", persistentScreeningsEenheid.getNaam());

			controleerAfsprakenInGewijzigdePeriodeMetPrognose(afsprakenBuitenStandplaatsPeriodeMap, screeningsEenheid, persistentScreeningsEenheid);

			try
			{
				if (afsprakenBuitenStandplaatsPeriodeMap.isEmpty())
				{
					verwijderCapaciteitblokken(runDry, meldingenDto, screeningsEenheid);
					wijzigCapaciteitblokken(runDry, meldingenDto, screeningsEenheid, persistentScreeningsEenheid, nieuweBlokken);
					wijzigStandplaatsPerioden(runDry, meldingenDto, screeningsEenheid, persistentScreeningsEenheid);
					wijzigSceeningsEenheid(runDry, meldingenDto, screeningsEenheid, persistentScreeningsEenheid);
				}
			}
			catch (MaxMeldingenVoorSeBereiktException e)
			{

			}
		}
		if (!afsprakenBuitenStandplaatsPeriodeMap.isEmpty())
		{
			throw new OpslaanAfsprakenBuitenStandplaatsPeriodeException(afsprakenBuitenStandplaatsPeriodeMap);
		}
		if (!runDry && meldingenDto.niveau != MammaMeldingNiveau.PROBLEEM)
		{
			screeningsOrganisatie.resetConceptGewijzigdDoor();
			nieuweBlokken.forEach((k, v) -> v.setId(k));
		}
		else
		{
			throw new DryRunException(meldingenDto);
		}
		return meldingenDto;
	}

	@Override
	public int getAantalAfsprakenTeOntkoppelen(MammaCapaciteitBlok blok, Date vanaf, Date tot, MammaCapaciteitBlokType nieuweBlokType)
	{
		var aantalAfspraken = 0;
		for (var afspraak : blok.getAfspraken())
		{
			if (moetOntkoppeldWorden(afspraak, vanaf, tot, nieuweBlokType))
			{
				aantalAfspraken++;
			}
		}
		return aantalAfspraken;
	}

	private void wijzigSceeningsEenheid(boolean runDry, PlanningConceptMeldingenDto meldingenDto, PlanningScreeningsEenheid screeningsEenheid,
		MammaScreeningsEenheid persistentScreeningsEenheid)
	{
		persistentScreeningsEenheid.setInterval(screeningsEenheid.getInterval());
		persistentScreeningsEenheid.setHerhalingsWeek(DateUtil.toUtilDate(screeningsEenheid.getHerhalingsWeek().getDatum()));
		var diffScreeningsEenheidToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(persistentScreeningsEenheid, hibernateService.getHibernateSession());
		if (diffScreeningsEenheidToLatestVersion.length() > 0)
		{
			var melding = "Gewijzigd: " + diffScreeningsEenheidToLatestVersion;
			addMelding(meldingenDto, screeningsEenheid, melding, MammaMeldingNiveau.INFO, runDry);
		}
		if (!runDry)
		{
			PlanningBlokIndex.reset(screeningsEenheid);
			screeningsEenheid.setInitieelInterval(screeningsEenheid.getInterval());
			screeningsEenheid.setInitieelHerhalingsWeek(screeningsEenheid.getHerhalingsWeek());
			hibernateService.saveOrUpdate(persistentScreeningsEenheid);
		}
	}

	private void wijzigStandplaatsPerioden(boolean runDry, PlanningConceptMeldingenDto meldingenDto, PlanningScreeningsEenheid screeningsEenheid,
		MammaScreeningsEenheid persistentScreeningsEenheid)
	{
		List<MammaAfspraak> teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode = new ArrayList<>();
		for (var standplaatsPeriode : screeningsEenheid.getStandplaatsPeriodeNavigableSet())
		{
			var melding = "";
			MammaStandplaatsPeriode persistentStandplaatsPeriode = null;
			MammaStandplaatsRonde persistentStandplaatsRonde = null;
			if (standplaatsPeriode.getId() != null)
			{
				persistentStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, standplaatsPeriode.getId());
				persistentStandplaatsRonde = persistentStandplaatsPeriode.getStandplaatsRonde();
			}
			var standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();
			if (persistentStandplaatsPeriode == null)
			{
				var persistentStandplaats = hibernateService.get(MammaStandplaats.class, standplaatsRonde.getStandplaats().getId());
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

			corrigeerScreeningsEenheidAlsNodig(persistentScreeningsEenheid, persistentStandplaatsPeriode);
			var verplaatsAfspraken = moetAfsprakenVerplaatsenVoorActieveStandplaats(runDry, meldingenDto, teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode,
				standplaatsPeriode, persistentStandplaatsPeriode);

			var diffStandplaatsPeriodeToLatestVersion = wijzigStandplaatsPeriode(standplaatsPeriode, persistentStandplaatsPeriode);
			var diffStandplaatsRondeToLatestVersion = wijzigStandplaatsRonde(standplaatsRonde, persistentStandplaatsRonde);

			if (diffStandplaatsPeriodeToLatestVersion.length() > 0 || diffStandplaatsRondeToLatestVersion.length() > 0)
			{
				if (melding.isEmpty())
				{
					melding = "Gewijzigd ";
				}
				melding += persistentStandplaatsPeriode.getStandplaatsRonde().getStandplaats().getNaam() + " / " + diffStandplaatsPeriodeToLatestVersion + " / "
					+ diffStandplaatsRondeToLatestVersion;
			}
			addMelding(meldingenDto, screeningsEenheid, melding, MammaMeldingNiveau.INFO, runDry);

			if (!runDry)
			{
				persistentStandplaatsRonde.setInterval(standplaatsRonde.getInterval());
				standplaatsRonde.setIntieelInterval(standplaatsRonde.getInterval());
				hibernateService.saveOrUpdateAll(persistentStandplaatsRonde, persistentStandplaatsRonde.getStandplaats());
				hibernateService.saveOrUpdate(persistentStandplaatsPeriode);

				PlanningStandplaatsRondeIndex.put(standplaatsRonde);
				standplaatsRonde.setId(persistentStandplaatsRonde.getId());
				standplaatsPeriode.setId(persistentStandplaatsPeriode.getId());
				conceptModelService.addStandplaatsPeriode(standplaatsPeriode);
				if (verplaatsAfspraken)
				{
					meldingenDto.afsprakenTeVerplaatsen.put(persistentStandplaatsPeriode.getId(),
						teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode.stream().map(AbstractHibernateObject::getId).collect(Collectors.toList()));
					teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode.clear();
				}
			}
		}
	}

	private boolean moetAfsprakenVerplaatsenVoorActieveStandplaats(boolean runDry, PlanningConceptMeldingenDto meldingenDto,
		List<MammaAfspraak> teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode, PlanningStandplaatsPeriode standplaatsPeriode,
		MammaStandplaatsPeriode persistentStandplaatsPeriode)
	{
		var verplaatsAfspraken = false;

		if (!teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode.isEmpty())
		{
			verplaatsAfspraken = true;
			addMelding(meldingenDto, standplaatsPeriode.getScreeningsEenheid(),
				"Er worden afspraken (#" + teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode.size() + ") verplaatst van standplaats "
					+ teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode.get(0).getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getNaam() + " naar standplaats "
					+ persistentStandplaatsPeriode.getStandplaatsRonde().getStandplaats().getNaam()
					+ ". Na 'Doorgaan' (opslaan van het concept) worden direct automatisch deze afspraken verzet en de brieven klaargezet klaargezet in 'Document afdrukken'. Dit kan echter nog een moment duren.",
				MammaMeldingNiveau.INFO, runDry);
		}

		var isActieveStandplaatsPeriodeVerkort = persistentStandplaatsPeriode.getId() != null
			&& baseStandplaatsService.isActieveStandplaatsPeriodeVerkort(persistentStandplaatsPeriode, standplaatsPeriode.getTotEnMet());
		if (isActieveStandplaatsPeriodeVerkort)
		{
			teVerplaatsenAfsprakenVoorActieveStandplaatsPeriode.addAll(baseAfspraakService.getAfspraken(persistentStandplaatsPeriode.getScreeningsEenheid(),
				standplaatsPeriode.getTotEnMet().plusDays(1), DateUtil.toLocalDate(persistentStandplaatsPeriode.getTotEnMet()),
				MammaAfspraakStatus.GEPLAND));
		}
		return verplaatsAfspraken;
	}

	private String wijzigStandplaatsRonde(PlanningStandplaatsRonde standplaatsRonde, MammaStandplaatsRonde persistentStandplaatsRonde)
	{
		persistentStandplaatsRonde.setAfspraakDrempel(standplaatsRonde.getAfspraakDrempel());
		persistentStandplaatsRonde.setAchtervangToegepast(standplaatsRonde.getAchtervangToegepast());
		persistentStandplaatsRonde.setMinderValideUitnodigenVanaf(DateUtil.toUtilDate(standplaatsRonde.getMinderValideUitnodigenVanaf()));

		var achtervangStandplaats = standplaatsRonde.getAchtervangStandplaats() != null
			? hibernateService.get(MammaStandplaats.class, standplaatsRonde.getAchtervangStandplaats().getId())
			: null;
		persistentStandplaatsRonde.setAchtervangStandplaats(achtervangStandplaats);

		var minderValideUitwijkStandplaats = standplaatsRonde.getMinderValideUitwijkStandplaats() != null
			? hibernateService.get(MammaStandplaats.class, standplaatsRonde.getMinderValideUitwijkStandplaats().getId())
			: null;
		persistentStandplaatsRonde.setMinderValideUitwijkStandplaats(minderValideUitwijkStandplaats);
		wijzigAfspraakcapaciteitBeschikbaarVoor(persistentStandplaatsRonde, standplaatsRonde.getAfspraakcapaciteitBeschikbaarVoor());

		return EntityAuditUtil.getDiffToLatestVersion(persistentStandplaatsRonde, hibernateService.getHibernateSession());
	}

	private String wijzigStandplaatsPeriode(PlanningStandplaatsPeriode standplaatsPeriode, MammaStandplaatsPeriode persistentStandplaatsPeriode)
	{
		persistentStandplaatsPeriode.setPrognose(standplaatsPeriode.getPrognose());
		persistentStandplaatsPeriode.setTotEnMet(DateUtil.toUtilDate(standplaatsPeriode.getTotEnMet()));
		persistentStandplaatsPeriode.setVanaf(DateUtil.toUtilDate(standplaatsPeriode.getVanaf()));
		persistentStandplaatsPeriode.setScreeningsEenheidVolgNr(standplaatsPeriode.getScreeningsEenheidVolgNr());
		persistentStandplaatsPeriode.setStandplaatsRondeVolgNr(standplaatsPeriode.getStandplaatsRondeVolgNr());

		return EntityAuditUtil.getDiffToLatestVersion(persistentStandplaatsPeriode, hibernateService.getHibernateSession());
	}

	private void corrigeerScreeningsEenheidAlsNodig(MammaScreeningsEenheid persistentScreeningsEenheid, MammaStandplaatsPeriode persistentStandplaatsPeriode)
	{
		var otherPersistentScreeningsEenheid = persistentStandplaatsPeriode.getScreeningsEenheid();
		if (!otherPersistentScreeningsEenheid.equals(persistentScreeningsEenheid))
		{
			otherPersistentScreeningsEenheid.getStandplaatsPerioden().remove(persistentStandplaatsPeriode);
			persistentStandplaatsPeriode.setScreeningsEenheid(persistentScreeningsEenheid);

			persistentScreeningsEenheid.getStandplaatsPerioden().add(persistentStandplaatsPeriode);
		}
	}

	private void wijzigCapaciteitblokken(boolean runDry, PlanningConceptMeldingenDto meldingenDto, PlanningScreeningsEenheid screeningsEenheid,
		MammaScreeningsEenheid persistentScreeningsEenheid,
		Map<Long, PlanningBlok> nieuweBlokken)
	{
		Set<MammaCapaciteitBlok> ontkoppelAfsprakenCapaciteitBlokken = new HashSet<>();
		Set<MammaCapaciteitBlok> koppelAfsprakenCapaciteitBlokken = new HashSet<>();
		Map<MammaCapaciteitBlok, PlanningMeldingDto> capaciteitBlokMeldingMap = new HashMap<>();

		try
		{
			for (var blok : PlanningBlokIndex.getBlokChangedSet(screeningsEenheid))
			{
				MammaCapaciteitBlok persistentBlok = null;
				var isNieuw = blok.getId() == null;
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

				var waarschuwing = "";
				var ontkoppelAfspraken = false;
				var aantalAfsprakenTeVerwijderen = getAantalAfsprakenTeOntkoppelen(persistentBlok, blok.getDateVanaf(), blok.getDateTot(), blok.getCapaciteitBlokType());
				if (aantalAfsprakenTeVerwijderen > 0)
				{
					waarschuwing = " Blok type of tijden zijn gewijzigd. Gekoppelde afspraken (#" + aantalAfsprakenTeVerwijderen + ") worden losgemaakt van dit capaciteitblok. ";
					ontkoppelAfspraken = true;
				}
				persistentBlok.setAantalOnderzoeken(blok.getAantalOnderzoeken());
				persistentBlok.setBlokType(blok.getCapaciteitBlokType());
				persistentBlok.setOpmerkingen(blok.getOpmerkingen());
				persistentBlok.setTot(blok.getDateTot());
				persistentBlok.setVanaf(blok.getDateVanaf());
				persistentBlok.setMinderValideAfspraakMogelijk(blok.isMinderValideAfspraakMogelijk());

				var melding = "";

				if (isNieuw)
				{
					melding += "Capaciteit aangemaakt (" + Constants.getDateTimeFormat().format(blok.getDateVanaf()) + ").";
				}
				else
				{
					var diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(persistentBlok, hibernateService.getHibernateSession());
					if (!diffToLatestVersion.isEmpty())
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
						if (isNieuw)
						{
							nieuweBlokken.put(persistentBlok.getId(), blok);
						}
					}
					koppelAfsprakenCapaciteitBlokken.add(persistentBlok);
					var niveau = MammaMeldingNiveau.INFO;
					if (!waarschuwing.isEmpty())
					{
						niveau = MammaMeldingNiveau.WAARSCHUWING;
					}
					addMelding(meldingenDto, screeningsEenheid, melding + waarschuwing, niveau, runDry);

					var seMeldingenDtoList = getMeldingenPerSeDto(meldingenDto, screeningsEenheid).meldingen;
					var meldingDto = seMeldingenDtoList.get(seMeldingenDtoList.size() - 1);
					capaciteitBlokMeldingMap.put(persistentBlok, meldingDto);

				}
			}

			ontkoppelAfsprakenCapaciteitBlokken.forEach(ontkoppelAfsprakenCapaciteitBlok -> ontkoppelAfspraken(ontkoppelAfsprakenCapaciteitBlok, false));
		}
		catch (MaxMeldingenVoorSeBereiktException e)
		{
			throw e;
		}
		finally
		{
			koppelAfsprakenCapaciteitBlokken.forEach(teKoppelenBlok ->
			{
				var waarschuwing = koppelNietGekoppeldeAfspraken(teKoppelenBlok, runDry);
				if (capaciteitBlokMeldingMap.containsKey(teKoppelenBlok))
				{
					var meldingDto = capaciteitBlokMeldingMap.get(teKoppelenBlok);
					meldingDto.tekst += waarschuwing;
				}

			});
		}

	}

	private void verwijderCapaciteitblokken(boolean runDry, PlanningConceptMeldingenDto meldingenDto, PlanningScreeningsEenheid screeningsEenheid)
	{
		for (var blokToRemove : PlanningBlokIndex.getBlokDeletedSet(screeningsEenheid))
		{
			if (blokToRemove.getId() != null)
			{
				LOG.info("Verwijder cap.blok " + blokToRemove.getCapaciteitBlokType() + " - " + blokToRemove.getVanaf());
				var persistentBlok = hibernateService.get(MammaCapaciteitBlok.class, blokToRemove.getId());
				if (persistentBlok != null)
				{
					var melding = "Capaciteit verwijderd (" + Constants.getDateTimeFormat().format(blokToRemove.getDateVanaf()) + ").";
					var niveau = MammaMeldingNiveau.INFO;
					if (!persistentBlok.getAfspraken().isEmpty())
					{
						melding += ". Gekoppelde afspraken (#" + persistentBlok.getAfspraken().size() + ") worden losgemaakt van dit capaciteitblok.";
						niveau = MammaMeldingNiveau.WAARSCHUWING;
					}
					addMelding(meldingenDto, screeningsEenheid, melding, niveau, runDry);

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
	}

	private void controleerAfsprakenInGewijzigdePeriodeMetPrognose(Map<Long, Date[]> afsprakenBuitenStandplaatsPeriodeMap, PlanningScreeningsEenheid screeningsEenheid,
		MammaScreeningsEenheid persistentScreeningsEenheid)
	{
		var eersteStandplaatsPeriodeMetPrognose = screeningsEenheid.getStandplaatsPeriodeNavigableSet().stream()
			.filter(PlanningStandplaatsPeriode::getPrognose)
			.findFirst().orElse(null);

		if (eersteStandplaatsPeriodeMetPrognose != null && eersteStandplaatsPeriodeMetPrognose.getId() != null)
		{
			var persistentStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, eersteStandplaatsPeriodeMetPrognose.getId());
			var conceptTotEnMet = eersteStandplaatsPeriodeMetPrognose.getTotEnMet();
			var persistentTotEnMet = DateUtil.toLocalDate(persistentStandplaatsPeriode.getTotEnMet());
			if (conceptTotEnMet.compareTo(persistentTotEnMet) != 0)
			{
				var oudeEnNieuweStandplaatsPeriodeTotEnMetDatum = Arrays.asList(conceptTotEnMet, persistentTotEnMet);
				var zoekAfsprakenVanafDatum = Collections.min(oudeEnNieuweStandplaatsPeriodeTotEnMetDatum).plusDays(1);
				var zoekAfsprakenTotEnMetDatum = Collections.max(oudeEnNieuweStandplaatsPeriodeTotEnMetDatum);
				var eersteEnLaatsteAfspraakVanaf = baseAfspraakDao.getEersteEnLaatsteAfspraakMomenten(persistentStandplaatsPeriode.getId(),
					zoekAfsprakenVanafDatum, zoekAfsprakenTotEnMetDatum, MammaAfspraakStatus.GEPLAND);

				if (eersteEnLaatsteAfspraakVanaf[0] != null)
				{
					var melding = "Concept kan niet worden opgeslagen voor SE " + persistentScreeningsEenheid.getNaam() + ". Standplaatsperiode "
						+ eersteStandplaatsPeriodeMetPrognose.getId() + " heeft afspraken op " + DateUtil.formatShortDate(eersteEnLaatsteAfspraakVanaf[0]) + (" t/m "
						+ DateUtil.formatShortDate(eersteEnLaatsteAfspraakVanaf[1]));
					LOG.warn(melding);
					afsprakenBuitenStandplaatsPeriodeMap.put(persistentScreeningsEenheid.getId(), eersteEnLaatsteAfspraakVanaf);
				}
			}
		}
	}

	private void wijzigAfspraakcapaciteitBeschikbaarVoor(MammaStandplaatsRonde persistentStandplaatsRonde, List<PlanningScreeningsOrganisatie> afspraakcapaciteitBeschikbaarVoor)
	{
		List<ScreeningOrganisatie> sosToDelete = new ArrayList<>();
		var persistentAfspraakcapaciteitBeschikbaarVoor = persistentStandplaatsRonde.getAfspraakcapaciteitBeschikbaarVoor();
		for (var persistentScreeningorganisatie : persistentAfspraakcapaciteitBeschikbaarVoor)
		{
			if (afspraakcapaciteitBeschikbaarVoor.stream().noneMatch(so -> so.getId().equals(persistentScreeningorganisatie.getId())))
			{
				sosToDelete.add(persistentScreeningorganisatie);
			}
		}
		persistentAfspraakcapaciteitBeschikbaarVoor.removeAll(sosToDelete);
		for (var screeningorganisatie : afspraakcapaciteitBeschikbaarVoor)
		{
			if (persistentAfspraakcapaciteitBeschikbaarVoor.stream().noneMatch(so -> so.getId().equals(screeningorganisatie.getId())))
			{
				persistentAfspraakcapaciteitBeschikbaarVoor.add(hibernateService.load(ScreeningOrganisatie.class, screeningorganisatie.getId()));
			}
		}
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
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
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
		var aanvullendeMelding = "";
		var aantalAfspraken = baseAfspraakService.koppelNietGekoppeldeAfspraken(persistentBlok, runDry);
		if (aantalAfspraken > 0)
		{
			aanvullendeMelding += aantalAfspraken + " nog niet gekoppelde afspraken worden gekoppeld aan dit capaciteitblok.";
		}
		return aanvullendeMelding;
	}

	private void ontkoppelAfspraken(MammaCapaciteitBlok persistentBlok, boolean delete)
	{
		for (var afspraak : persistentBlok.getAfspraken())
		{
			if (delete || moetOntkoppeldWorden(afspraak, persistentBlok.getVanaf(), persistentBlok.getTot(), persistentBlok.getBlokType()))
			{
				afspraak.setCapaciteitBlok(null);
				hibernateService.saveOrUpdate(afspraak);
				LOG.info("Afspraak van {} voor clientId {} ontkoppeld van cap.blok", afspraak.getVanaf(),
					afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient().getId());
			}
		}
		persistentBlok.getAfspraken().clear();
	}

	private void addMelding(PlanningConceptMeldingenDto meldingenDto, PlanningScreeningsEenheid screeningsEenheid, String melding, MammaMeldingNiveau niveau, boolean runDry)
	{
		if (StringUtils.isBlank(melding))
		{
			return;
		}
		var maxMeldingVoorSeBereikt = false;
		var meldingDto = new PlanningMeldingDto();
		meldingDto.tekst = melding;
		meldingDto.niveau = niveau;
		var screeningsEenheidId = screeningsEenheid.getId();

		LOG.info(screeningsEenheidId + " " + niveau + ": " + melding);

		var meldingenPerSeDto = getMeldingenPerSeDto(meldingenDto, screeningsEenheid);
		if (meldingenPerSeDto == null)
		{
			meldingenPerSeDto = new PlanningMeldingenPerSeDto();
			meldingenPerSeDto.niveau = niveau;
			meldingenPerSeDto.screeningsEenheidId = screeningsEenheidId;
			meldingenDto.seMeldingen.put(screeningsEenheidId, meldingenPerSeDto);
		}
		var meldingenPerSe = meldingenPerSeDto.meldingen;
		if (meldingenPerSe.size() >= PlanningConstanten.MAX_MELDINGEN_PER_SE)
		{
			if (MAX_AANTAL_MELDINGEN_BEREIKT_MELDING.equals(meldingenPerSe.get(meldingenPerSe.size() - 1).tekst))
			{

				return;
			}
			else
			{
				meldingDto = new PlanningMeldingDto();
				meldingDto.tekst = MAX_AANTAL_MELDINGEN_BEREIKT_MELDING;
				meldingDto.niveau = MammaMeldingNiveau.WAARSCHUWING;
				maxMeldingVoorSeBereikt = true;
			}
		}
		meldingenPerSe.add(meldingDto);

		if (meldingenDto.niveau == null || meldingenDto.niveau.ordinal() < niveau.ordinal())
		{
			meldingenDto.niveau = niveau;
		}
		if (meldingenPerSeDto.niveau == null || meldingenPerSeDto.niveau.ordinal() < niveau.ordinal())
		{
			meldingenPerSeDto.niveau = niveau;
		}
		if (maxMeldingVoorSeBereikt && runDry)
		{
			throw new MaxMeldingenVoorSeBereiktException();
		}
	}

}

package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.dao.mamma.MammaStandplaatsPeriodeDao;
import nl.rivm.screenit.main.exception.MagOpslaanException;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeUitnodigenRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

@Service
public class MammaStandplaatsPeriodeServiceImpl implements MammaStandplaatsPeriodeService
{
	@Autowired
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private MammaStandplaatsPeriodeDao standplaatsPeriodeDao;

	@Autowired
	private MammaBaseStandplaatsService baseStandplaatsService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Autowired
	private MammaBaseAfspraakService baseAfspraakService;

	@Override
	public List<PlanningStandplaatsPeriodeDto> getStandplaatsPeriodesSorted(MammaScreeningsEenheid screeningsEenheid)
	{
		return new ArrayList<>(Arrays.asList(baseConceptPlanningsApplicatie.getStandplaatsPeriodesSorted(screeningsEenheid)));
	}

	@Override
	public List<MammaStandplaatsPeriode> getStandplaatsPeriodesVoorBulkVerzetten(ScreeningOrganisatie regio)
	{
		Date verzettenVanaf = DateUtil.plusWerkdagen(currentDateSupplier.getDateMidnight(),
			simplePreferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN.name()));

		return standplaatsPeriodeDao.getStandplaatsPeriodesVoorBulkVerzetten(regio, verzettenVanaf);
	}

	@Override
	@Transactional
	public void splitsStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		baseConceptPlanningsApplicatie.splitsStandplaatsPeriode(standplaatsPeriode, ingelogdeInstellingGebruiker);
	}

	@Override
	@Transactional
	public void updateSortList(int nieuwVolgnummer, PlanningStandplaatsPeriodeDto item, MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		item.screeningsEenheidVolgNr = nieuwVolgnummer;
		baseConceptPlanningsApplicatie.changeRoute(item, screeningsEenheid, ingelogdeInstellingGebruiker);
	}

	@Override
	@Transactional
	public boolean saveOrUpdateStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		baseConceptPlanningsApplicatie.sendAfspraakDrempelStandplaatsPeriode(standplaatsPeriode, ingelogdeInstellingGebruiker);
		return true;
	}

	@Override
	public MammaStandplaatsRondeUitnodigenRapportage getStandplaatsRondeUitnodigenRapportage(MammaStandplaatsRonde standplaatsRonde)
	{
		return standplaatsPeriodeDao.getStandplaatsRondeUitnodigenRapportage(standplaatsRonde);
	}

	@Override
	public List<MammaStandplaats> getStandplaatsenBuitenRegio(IMammaAfspraakWijzigenFilter filter, boolean uitstellen)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = baseStandplaatsService.getStandplaatsPeriodeMetAfstandDtos(filter, uitstellen);

		Set<MammaStandplaats> standplaatsenBuitenRegio = new HashSet<>();
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null || uitstellen)
			{
				standplaatsenBuitenRegio.add(standplaatsPeriode.getStandplaatsRonde().getStandplaats());
			}
		}
		return new ArrayList<>(standplaatsenBuitenRegio);
	}

	@Override
	public List<MammaScreeningsEenheid> getScreeningEenhedenBuitenRegio(IMammaAfspraakWijzigenFilter filter, boolean uitstellen)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = baseStandplaatsService.getStandplaatsPeriodeMetAfstandDtos(filter, uitstellen);

		Set<MammaScreeningsEenheid> standplaatsenBuitenRegio = new HashSet<>();
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null || uitstellen)
			{
				standplaatsenBuitenRegio.add(standplaatsPeriode.getScreeningsEenheid());
			}
		}
		return new ArrayList<>(standplaatsenBuitenRegio);
	}

	@Override
	public long countAfsprakenTeVerzetten(LocalDate nieuweEindDatum, PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, MammaScreeningsEenheid screeningsEenheid)
	{
		long aantalAfspraken = 0;

		if (standplaatsPeriodeDto.id == null)
		{
			return aantalAfspraken;
		}

		var persistentStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, standplaatsPeriodeDto.id);
		if (baseStandplaatsService.isActieveStandplaatsPeriodeVerkort(persistentStandplaatsPeriode, nieuweEindDatum))
		{
			var totEnMet = DateUtil.toLocalDate(persistentStandplaatsPeriode.getTotEnMet());
			aantalAfspraken = baseAfspraakService.countAfspraken(screeningsEenheid,
				nieuweEindDatum.plusDays(1), totEnMet, MammaAfspraakStatus.GEPLAND);
		}
		return aantalAfspraken;
	}

	@Override
	public void magOnthouden(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, boolean nieuwePrognose, LocalDate nieuweEindDatum,
		LocalDate vrijgegevenTotEnMet, LocalDate uitnodigenTotEnMet, PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode) throws MagOpslaanException
	{

		controleerLegeEindDatum(nieuweEindDatum);
		controleerLegeBeginDatum(standplaatsPeriodeDto);

		var prognoseAangezet = Boolean.FALSE.equals(standplaatsPeriodeDto.prognose) && nieuwePrognose;
		if (prognoseAangezet)
		{
			controleerAanzettenPrognoseMetAfsprakenInVolgendePeriode(volgendeStandplaatsPeriode);
			controleerAanzettenPrognoseBinnenVijfDagen(standplaatsPeriodeDto, vrijgegevenTotEnMet, uitnodigenTotEnMet);
			controleerGesplitsteEindDatum(standplaatsPeriodeDto);
		}

		if (!nieuwePrognose)
		{
			controleerEindDatumNaBeginDatum(nieuweEindDatum, standplaatsPeriodeDto);
			controleerEindDatumVoorVrijgegevenTotEnMet(nieuweEindDatum, vrijgegevenTotEnMet, volgendeStandplaatsPeriode);
			controleerEindDatumVoorUitnodigenTotEnMet(nieuweEindDatum, uitnodigenTotEnMet, volgendeStandplaatsPeriode);
			controleerInkortenNietLopendeStandplaatsPeriode(nieuweEindDatum, standplaatsPeriodeDto);
			controleerVolgendeStandplaatsPeriode(nieuweEindDatum, volgendeStandplaatsPeriode);
			controleerAfspraakInVolgendeStandplaatsPeriode(nieuweEindDatum, volgendeStandplaatsPeriode);
		}

		controleerPrognoseVolgendeStandplaats(nieuwePrognose, volgendeStandplaatsPeriode);
		controleerAchtervangStandplaats(nieuwePrognose, standplaatsPeriodeDto);
	}

	private void controleerLegeEindDatum(LocalDate nieuweEindDatum) throws MagOpslaanException
	{
		if (nieuweEindDatum == null)
		{
			throw new MagOpslaanException("Standplaatsperiode.lege.einddatum");
		}
	}

	private void controleerLegeBeginDatum(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto) throws MagOpslaanException
	{
		if (standplaatsPeriodeDto.vanaf == null)
		{
			throw new MagOpslaanException("Standplaatsperiode.lege.begindatum");
		}
	}

	private void controleerAanzettenPrognoseMetAfsprakenInVolgendePeriode(PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode)
		throws MagOpslaanException
	{
		if (volgendeStandplaatsPeriode != null)
		{
			var datumEersteAfspraakVolgendePeriode = afspraakService.getDatumEersteGeplandeAfspraak(volgendeStandplaatsPeriode.id);
			if (datumEersteAfspraakVolgendePeriode != null)
			{
				throw new MagOpslaanException("Standplaatsperiode.aanzetten.prognose.leeg.afspraken.in.volgende");
			}
		}
	}

	private void controleerAanzettenPrognoseBinnenVijfDagen(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto,
		LocalDate vrijgegevenTotEnMet, LocalDate uitnodigenTotEnMet) throws MagOpslaanException
	{
		if (vrijgegevenTotEnMet != null && !MammaPlanningUtil.datumIsMeerDanVijfWerkdagenVoorDatum(vrijgegevenTotEnMet, standplaatsPeriodeDto.totEnMet)
			|| uitnodigenTotEnMet != null && !MammaPlanningUtil.datumIsMeerDanVijfWerkdagenVoorDatum(uitnodigenTotEnMet, standplaatsPeriodeDto.totEnMet))
		{
			throw new MagOpslaanException("Standplaatsperiode.aanzetten.prognose.binnen.vijf.dagen");
		}
	}

	private void controleerGesplitsteEindDatum(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto) throws MagOpslaanException
	{
		if (Boolean.TRUE.equals(standplaatsPeriodeDto.gesplitst))
		{
			throw new MagOpslaanException("Standplaatsperiode.aanzetten.prognose.gesplitst");
		}
	}

	private void controleerEindDatumNaBeginDatum(LocalDate nieuweEindDatum, PlanningStandplaatsPeriodeDto standplaatsPeriodeDto) throws MagOpslaanException
	{
		if (nieuweEindDatum.isBefore(standplaatsPeriodeDto.vanaf))
		{
			throw new MagOpslaanException("Standplaatsperiode.einddatum.veranderen.eind.voor.begin");
		}
	}

	private void controleerEindDatumVoorVrijgegevenTotEnMet(LocalDate nieuweEindDatum, LocalDate vrijgegevenTotEnMet,
		PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode) throws MagOpslaanException
	{
		if (volgendeStandplaatsPeriode == null && vrijgegevenTotEnMet != null && nieuweEindDatum.isBefore(vrijgegevenTotEnMet))
		{
			throw new MagOpslaanException("Standplaatsperiode.einddatum.voor.vrijgegeven.tot.en.met");
		}
	}

	private void controleerEindDatumVoorUitnodigenTotEnMet(LocalDate nieuweEindDatum, LocalDate uitnodigenTotEnMet,
		PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode) throws MagOpslaanException
	{
		if (volgendeStandplaatsPeriode == null && uitnodigenTotEnMet != null && nieuweEindDatum.isBefore(uitnodigenTotEnMet))
		{
			throw new MagOpslaanException("Standplaatsperiode.einddatum.voor.uitnodigen.tot.en.met");
		}
	}

	private void controleerInkortenNietLopendeStandplaatsPeriode(LocalDate nieuweEindDatum, PlanningStandplaatsPeriodeDto standplaatsPeriodeDto)
		throws MagOpslaanException
	{
		var datumLaatsteAfspraak = DateUtil.toLocalDate(afspraakService.getDatumLaatsteGeplandeAfspraak(standplaatsPeriodeDto.id));
		if (datumLaatsteAfspraak != null && datumLaatsteAfspraak.isAfter(nieuweEindDatum) && !isLopendeStandplaatsPeriode(standplaatsPeriodeDto.vanaf, nieuweEindDatum))
		{
			String datumLaatsteAfspraakText = datumLaatsteAfspraak.format(DateUtil.LOCAL_DATE_FORMAT);
			throw new MagOpslaanException("Standplaatsperiode.einddatum.veranderen.overschrijdt.afspraak.na", datumLaatsteAfspraakText);
		}
	}

	private void controleerVolgendeStandplaatsPeriode(LocalDate nieuweEindDatum, PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode)
		throws MagOpslaanException
	{
		if (volgendeStandplaatsPeriode != null && Boolean.FALSE.equals(volgendeStandplaatsPeriode.prognose) && !nieuweEindDatum.isBefore(volgendeStandplaatsPeriode.totEnMet))
		{
			throw new MagOpslaanException("Standplaatsperiode.einddatum.voor.volgende.einddatum");
		}
	}

	private void controleerAfspraakInVolgendeStandplaatsPeriode(LocalDate nieuweEindDatum, PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode)
		throws MagOpslaanException
	{
		if (volgendeStandplaatsPeriode != null)
		{
			var datumEersteAfspraakVolgendePeriode = DateUtil.toLocalDate(afspraakService.getDatumEersteGeplandeAfspraak(volgendeStandplaatsPeriode.id));
			if (datumEersteAfspraakVolgendePeriode != null && !nieuweEindDatum.isBefore(datumEersteAfspraakVolgendePeriode))
			{
				String datumEersteAfspraakVolgendePeriodeText = datumEersteAfspraakVolgendePeriode.format(DateUtil.LOCAL_DATE_FORMAT);
				throw new MagOpslaanException("Standplaatsperiode.einddatum.veranderen.overschrijdt.afspraak.voor", datumEersteAfspraakVolgendePeriodeText);
			}
		}
	}

	private void controleerPrognoseVolgendeStandplaats(boolean nieuwePrognose, PlanningStandplaatsPeriodeDto volgendeStandplaatsDto) throws MagOpslaanException
	{
		if (nieuwePrognose && volgendeStandplaatsDto != null && Boolean.FALSE.equals(volgendeStandplaatsDto.prognose))
		{
			throw new MagOpslaanException("Standplaatsperiode.prognose.niet.mogelijk.volgende.periode");
		}
	}

	private void controleerAchtervangStandplaats(boolean nieuwePrognose, PlanningStandplaatsPeriodeDto standplaatsPeriodeDto) throws MagOpslaanException
	{
		if (!nieuwePrognose && standplaatsPeriodeDto.achtervangStandplaatsId == null)
		{
			throw new MagOpslaanException("Standplaatsperiode.achtervang.verplicht");
		}
	}

	private boolean isLopendeStandplaatsPeriode(LocalDate beginDatum, LocalDate eindDatum)
	{
		return Range.closedOpen(beginDatum, eindDatum).contains(currentDateSupplier.getLocalDate());
	}
}

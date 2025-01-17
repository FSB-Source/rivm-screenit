package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonFeestdagService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.mappers.colon.ColonFeestdagMapper;
import nl.rivm.screenit.model.colon.ColonFeestdag;
import nl.rivm.screenit.model.colon.ColonFeestdag_;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.dto.ColonFeestdagDto;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonFeestdagRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.specification.colon.ColonFeestdagSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.joining;
import static nl.rivm.screenit.specification.colon.ColonFeestdagSpecification.heeftDatumInRange;
import static nl.rivm.screenit.specification.colon.ColonFeestdagSpecification.isActief;
import static nl.rivm.screenit.specification.colon.ColonFeestdagSpecification.isNietFeestdag;

@Service
@AllArgsConstructor
public class ColonFeestdagServiceImpl implements ColonFeestdagService
{
	@Autowired
	private LogService logService;

	@Autowired
	private RoosterService roosterService;

	@Autowired
	private ColonFeestdagRepository feestdagRepository;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private ColonFeestdagMapper feestdagMapper;

	@Override
	public List<ColonFeestdag> getFeestdagen()
	{
		var beginDitJaar = currentDateSupplier.getLocalDate().withDayOfYear(1);
		var specification = isActief().and(ColonFeestdagSpecification.heeftDatumVanaf(beginDitJaar));
		return feestdagRepository.findAll(specification, Sort.by(ColonFeestdag_.DATUM));
	}

	@Override
	public List<ColonFeestdag> getFeestdagen(String start, String eind)
	{
		var startDatum = DateUtil.parseLocalDateForPattern(start, "dd-MM-yyyy");
		var eindDatum = DateUtil.parseLocalDateForPattern(eind, "dd-MM-yyyy");
		var specification = isActief().and(heeftDatumInRange(startDatum, eindDatum));
		return feestdagRepository.findAll(specification, Sort.by(ColonFeestdag_.DATUM));
	}

	@Override
	public Optional<ColonFeestdag> getFeestdagById(Long id)
	{
		return feestdagRepository.findById(id);
	}

	@Override
	@Transactional
	public ColonFeestdag createFeestdag(ColonFeestdagDto feestdagDto) throws ValidatieException
	{
		valideerFeestdag(feestdagDto);

		var feestdag = feestdagMapper.colonFeestdagDtoToColonFeestdag(feestdagDto);
		logAction(String.format("Feestdag %s op %s is aangemaakt", feestdag.getNaam(), feestdag.getDatum()));
		return feestdagRepository.save(feestdag);
	}

	@Override
	@Transactional
	public ColonFeestdag updateFeestdag(Long id, ColonFeestdagDto feestdagDto) throws ValidatieException
	{
		var persistedFeestdag = getFeestdagById(id);
		if (persistedFeestdag.isEmpty())
		{
			throw new ValidatieException("error.feestdag.niet.gevonden");
		}

		valideerFeestdag(feestdagDto);

		var feestdag = persistedFeestdag.get();
		feestdag.setDatum(feestdagDto.getDatum());
		feestdag.setBeperking(feestdagDto.getBeperking());
		feestdag.setNaam(feestdagDto.getNaam());
		logAction(String.format("Feestdag %s op %s is gewijzigd", feestdag.getNaam(), feestdag.getDatum()));
		return feestdagRepository.save(feestdag);
	}

	@Override
	@Transactional
	public void deleteFeestdag(Long id) throws ValidatieException
	{
		var persistedFeestdag = getFeestdagById(id);
		if (persistedFeestdag.isEmpty())
		{
			throw new ValidatieException("error.feestdag.niet.gevonden");
		}
		var feestdag = persistedFeestdag.get();
		logAction(String.format("Feestdag %s op %s is verwijderd", feestdag.getNaam(), feestdag.getDatum()));
		feestdagRepository.deleteById(id);
	}

	private void logAction(String bericht)
	{
		var account = ScreenitSession.get().getLoggedInAccount();
		logService.logGebeurtenis(LogGebeurtenis.COLON_FEESTDAGEN_BEHEER, account, bericht, Bevolkingsonderzoek.COLON);
	}

	private void valideerFeestdag(ColonFeestdagDto feestdagDto) throws ValidatieException
	{
		heeftAfspraakslots(feestdagDto);
		overlaptMetBestaandeFeestdag(feestdagDto);
	}

	private void heeftAfspraakslots(ColonFeestdagDto feestdagDto) throws ValidatieException
	{
		var afspraakslots = getAfspraakslotsOpFeestdag(feestdagDto);
		if (!afspraakslots.isEmpty())
		{
			var afspraakslotsPerIntakelocatie = afspraakslots.stream().collect(groupingBy(afspraakslot -> afspraakslot.getKamer().getIntakelocatie()));
			var validatieMessage = afspraakslotsPerIntakelocatie.entrySet().stream().map(a -> formatHeeftAfspraakBericht(a.getKey(), a.getValue())).collect(joining("<br />"));
			throw new ValidatieException("error.feestdag.heeft.afspraken", validatieMessage);
		}
	}

	private String formatHeeftAfspraakBericht(ColonIntakelocatie intakelocatie, List<ColonAfspraakslot> afspraakslots)
	{
		var message = "<strong>" + intakelocatie.getNaam() + "</strong>";
		var afsprakenString = afspraakslots.stream()
			.map(afspraakslot -> DateUtil.formatShortDateTime(afspraakslot.getVanaf()) + "-" + DateUtil.formatLocalTime(afspraakslot.getTot())
			).collect(joining(", "));
		return message + ": " + afsprakenString;
	}

	private List<ColonAfspraakslot> getAfspraakslotsOpFeestdag(ColonFeestdagDto feestdag)
	{
		var startDag = feestdag.getDatum();
		var eindDag = feestdag.getDatum();
		var range = Range.closed(startDag, eindDag);
		return roosterService.getAfspraakslotsInRange(range);
	}

	private void overlaptMetBestaandeFeestdag(ColonFeestdagDto feestdagDto) throws ValidatieException
	{
		var heeftFeestdagenMetOverlap = feestdagRepository.exists(
			isActief()
				.and(heeftDatumInRange(feestdagDto.getDatum(), feestdagDto.getDatum()))
				.and(isNietFeestdag(feestdagDto))
		);

		if (heeftFeestdagenMetOverlap)
		{
			throw new ValidatieException("error.feestdag.heeft.overlap");
		}
	}
}

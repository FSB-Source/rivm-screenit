package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.BetalingDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingCsvDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingTotalenDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.model.Betaling;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Verrichting;
import nl.rivm.screenit.huisartsenportaal.repository.BetalingRepository;
import nl.rivm.screenit.huisartsenportaal.repository.VerrichtingCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.repository.VerrichtingRepository;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.VerrichtingenService;
import nl.rivm.screenit.huisartsenportaal.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import au.com.bytecode.opencsv.CSVWriter;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class VerrichtingServiceImpl implements VerrichtingenService
{
	@Autowired
	private VerrichtingCriteriaRepository verrichtingCriteriaRepository;

	@Autowired
	private VerrichtingRepository verrichtingRepository;

	@Autowired
	private BetalingRepository betalingRepository;

	@Autowired
	private LocatieService locatieService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public VerrichtingTotalenDto getVerrichtingen(Huisarts huisarts, VerrichtingZoekObjectDto verrichtingDto)
	{
		if (verrichtingDto.getResultOptions().getCount() == 0)
		{
			verrichtingDto.getResultOptions().setCount(10);
		}
		List<Verrichting> verrichtingen = verrichtingCriteriaRepository.getVerrichtingen(huisarts, verrichtingDto, verrichtingDto.getResultOptions());
		List<VerrichtingDto> dtos = new ArrayList<>();
		for (Verrichting verrichting : verrichtingen)
		{
			dtos.add(convertToDto(verrichting));
		}

		VerrichtingTotalenDto totalenDto = new VerrichtingTotalenDto();
		totalenDto.setVerrichtingen(dtos);
		totalenDto.setAantalVerrichtingen(verrichtingCriteriaRepository.countVerrichtingen(huisarts, verrichtingDto));
		return totalenDto;
	}

	@Override
	public File getVerrichtingenCsv(Huisarts huisarts, VerrichtingZoekObjectDto verrichtingDto) throws IOException
	{
		List<Verrichting> verrichtingen = verrichtingCriteriaRepository.getVerrichtingen(huisarts, verrichtingDto, verrichtingDto.getResultOptions());
		List<VerrichtingCsvDto> dtos = new ArrayList<>();
		for (Verrichting verrichting : verrichtingen)
		{
			dtos.add(convertToCsvDto(verrichting));
		}

		var csvFile = File.createTempFile("verrichtingen", ".csv");
		try (var writer = new FileWriter(csvFile, true);
			var csvWriter = new CSVWriter(writer, ';', CSVWriter.NO_QUOTE_CHARACTER, CSVWriter.DEFAULT_ESCAPE_CHARACTER, CSVWriter.DEFAULT_LINE_END))
		{
			var header = new String[] { "Huisartslocatie", "Naam cliënt", "Monster-id", "Datum uitstrijkje", "Datum verrichting", "Ontvangst formulier", "Screeningsorganisatie" };
			csvWriter.writeNext(header);

			dtos.forEach(verrichting ->
			{
				var line = new String[] { verrichting.getHuisartsLocatieNaam(), verrichting.getClientNaam(), verrichting.getMonsterId(), verrichting.getDatumUitstrijkje(),
					verrichting.getVerrichtingsDatum(), verrichting.getFormulierOntvangstDatum(), verrichting.getRegio() };
				csvWriter.writeNext(line);
			});
		}

		return csvFile;
	}

	@Override
	public Verrichting getVerrichting(VerrichtingDto verrichtingDto)
	{
		Verrichting verrichting = null;
		if (verrichtingDto.getHuisartsportaalId() != null)
		{
			verrichting = verrichtingRepository.findByHuisartsportaalId(verrichtingDto.getHuisartsportaalId());
		}
		if (verrichting == null && verrichtingDto.getScreenitId() != null)
		{
			verrichting = verrichtingRepository.findByScreenitId(verrichtingDto.getScreenitId());
		}
		return verrichting;
	}

	private VerrichtingCsvDto convertToCsvDto(Verrichting verrichting)
	{
		VerrichtingCsvDto verrichtingCsvDto = new VerrichtingCsvDto();
		verrichtingCsvDto.setHuisartsLocatieNaam(verrichting.getHuisartsLocatie().getNaam());
		verrichtingCsvDto.setClientNaam(verrichting.getClientNaam());
		verrichtingCsvDto.setMonsterId(verrichting.getMonsterId());
		verrichtingCsvDto.setVerrichtingsDatum(getPrintDatum(verrichting.getVerrichtingsDatum()));
		verrichtingCsvDto.setDatumUitstrijkje(getPrintDatum(verrichting.getDatumUitstrijkje()));
		verrichtingCsvDto.setFormulierOntvangstDatum(getPrintDatum(verrichting.getFormulierOntvangstDatum()));
		verrichtingCsvDto.setRegio(verrichting.getRegio());
		return verrichtingCsvDto;
	}

	@Override
	public String getPrintDatum(Date datum)
	{
		if (datum != null)
		{
			return DateUtil.formatForPattern("dd-MM-yyyy", datum);
		}
		return null;
	}

	@Override
	public VerrichtingDto convertToDto(Verrichting verrichting)
	{
		VerrichtingDto verrichtingDto = new VerrichtingDto();
		verrichtingDto.setHuisartsportaalId(verrichting.getHuisartsportaalId());
		verrichtingDto.setRegio(verrichting.getRegio());
		verrichtingDto.setMonsterId(verrichting.getMonsterId());
		verrichtingDto.setVerrichtingsDatum(verrichting.getVerrichtingsDatum());
		verrichtingDto.setDatumUitstrijkje(verrichting.getDatumUitstrijkje());
		verrichtingDto.setFormulierOntvangstDatum(verrichting.getFormulierOntvangstDatum());
		verrichtingDto.setHuisartsLocatie(locatieService.getLocatieDto(verrichting.getHuisartsLocatie()));
		verrichtingDto.setHuisartsLocatieNaam(verrichting.getHuisartsLocatie().getNaam());
		verrichtingDto.setClientNaam(verrichting.getClientNaam());
		return verrichtingDto;
	}

	@Override
	public Verrichting convertFromDto(VerrichtingDto verrichtingDto)
	{
		Verrichting verrichting = new Verrichting();
		verrichting.setScreenitId(verrichtingDto.getScreenitId());
		verrichting.setRegio(verrichtingDto.getRegio());
		verrichting.setMonsterId(verrichtingDto.getMonsterId());
		verrichting.setVerrichtingsDatum(verrichtingDto.getVerrichtingsDatum());
		verrichting.setDatumUitstrijkje(verrichtingDto.getDatumUitstrijkje());
		verrichting.setFormulierOntvangstDatum(verrichtingDto.getFormulierOntvangstDatum());
		verrichting.setClientNaam(verrichtingDto.getClientNaam());
		verrichting.setHuisartsLocatie(locatieService.getLocatie(verrichtingDto.getHuisartsLocatie()));
		verrichting.setHuisarts(verrichting.getHuisartsLocatie().getHuisarts());
		return verrichting;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Verrichting saveScreenITVerrichting(VerrichtingDto verrichtingDto)
	{
		Verrichting verrichting = verrichtingRepository.findByScreenitId(verrichtingDto.getScreenitId());
		List<Betaling> nieuweBetalingen = new ArrayList<>();

		if (verrichting == null)
		{
			verrichting = convertFromDto(verrichtingDto);
		}

		for (BetalingDto betalingDto : verrichtingDto.getBetalingen())
		{
			Betaling betaling = betalingRepository.findByScreenitId(betalingDto.getScreenitId());

			if (betaling != null)
			{
				updateBetaling(betalingDto, betaling);
			}
			else
			{

				if (verrichting.getBetalingen().size() == 1)
				{
					Betaling bestaandeBetaling = verrichting.getBetalingen().get(0);
					if (bestaandeBetaling.getScreenitId() == null)
					{
						BetalingDto oudsteBetaling = getOudsteBetalingDto(verrichtingDto);
						betaling = updateBetaling(oudsteBetaling, bestaandeBetaling);
					}
				}
				if (betaling == null)
				{
					betaling = convertFromDto(betalingDto);
					nieuweBetalingen.add(betaling);
				}
			}
		}

		verrichtingRepository.save(verrichting);
		for (Betaling betaling : nieuweBetalingen)
		{
			betaling.setVerrichting(verrichting);
			betalingRepository.save(betaling);
		}
		verrichting.getBetalingen().addAll(nieuweBetalingen);
		verrichtingRepository.save(verrichting);
		return verrichting;
	}

	private Betaling updateBetaling(BetalingDto betalingDto, Betaling betaling)
	{
		betaling.setBetalingsdatum(betalingDto.getBetalingsdatum());
		betaling.setBetalingsKenmerk(betalingDto.getBetalingsKenmerk());
		betaling.setBedrag(betalingDto.getBedrag());
		betaling.setScreenitId(betalingDto.getScreenitId());
		betalingRepository.save(betaling);
		return betaling;
	}

	private BetalingDto getOudsteBetalingDto(VerrichtingDto verrichtingDto)
	{
		BetalingDto oudsteBetalingDto = new BetalingDto();
		oudsteBetalingDto.setScreenitId(Long.MAX_VALUE);

		for (BetalingDto betalingDto : verrichtingDto.getBetalingen())
		{
			if (betalingDto.getScreenitId() < oudsteBetalingDto.getScreenitId())
			{
				oudsteBetalingDto = betalingDto;
			}
		}
		return oudsteBetalingDto;
	}

	private Betaling convertFromDto(BetalingDto betalingDto)
	{
		Betaling betaling = new Betaling();
		betaling.setScreenitId(betalingDto.getScreenitId());
		betaling.setDebet(betalingDto.isDebet());
		betaling.setBetalingsKenmerk(betalingDto.getBetalingsKenmerk());
		betaling.setBetalingsdatum(betalingDto.getBetalingsdatum());
		betaling.setBedrag(betalingDto.getBedrag());
		return betaling;
	}

}

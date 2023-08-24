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
import java.math.BigDecimal;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.huisartsenportaal.dto.BetalingCsvDto;
import nl.rivm.screenit.huisartsenportaal.dto.BetalingDto;
import nl.rivm.screenit.huisartsenportaal.dto.BetalingZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.dto.BetalingenTotalenDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingDto;
import nl.rivm.screenit.huisartsenportaal.model.Betaling;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Verrichting;
import nl.rivm.screenit.huisartsenportaal.repository.BetalingCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.service.BetalingService;
import nl.rivm.screenit.huisartsenportaal.service.VerrichtingenService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import au.com.bytecode.opencsv.CSVWriter;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class BetalingServiceImpl implements BetalingService
{
	@Autowired
	private BetalingCriteriaRepository betalingCriteriaRepository;

	@Autowired
	private VerrichtingenService verrichtingenService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public BetalingenTotalenDto getBetalingen(Huisarts huisarts, BetalingZoekObjectDto betalingZoekObjectDto)
	{
		if (betalingZoekObjectDto.getResultOptions().getCount() == 0)
		{
			betalingZoekObjectDto.getResultOptions().setCount(10);
		}
		List<Betaling> betalingen = betalingCriteriaRepository.getBetalingen(huisarts, betalingZoekObjectDto);
		List<BetalingDto> dtos = new ArrayList<>();
		if (betalingen != null)
		{
			dtos = betalingen.stream().map(betaling -> convertToDisplayDto(betaling, betaling.getVerrichting())).collect(Collectors.toList());
		}
		BetalingenTotalenDto totalenDto = new BetalingenTotalenDto();
		totalenDto.setBetalingen(dtos);
		totalenDto.setTotaalBedrag(NumberFormat.getCurrencyInstance().format(getTotaalBedrag(huisarts, betalingZoekObjectDto)));
		totalenDto.setAantalBetalingen(betalingCriteriaRepository.countBetalingen(huisarts, betalingZoekObjectDto));
		return totalenDto;
	}

	@Override
	public File getBetalingenCsv(Huisarts huisarts, BetalingZoekObjectDto betalingDto) throws IOException
	{
		List<Betaling> betalingen = betalingCriteriaRepository.getBetalingen(huisarts, betalingDto);
		var dtos = betalingen.stream()
			.map(this::convertToCsvDto)
			.collect(Collectors.toList());

		var csvFile = File.createTempFile("betalingen", ".csv");
		try (var writer = new FileWriter(csvFile, true);
			var csvWriter = new CSVWriter(writer, ';', CSVWriter.NO_QUOTE_CHARACTER, CSVWriter.DEFAULT_ESCAPE_CHARACTER, CSVWriter.DEFAULT_LINE_END))
		{
			var header = new String[] { "Huisartslocatie", "Naam cliënt", "Monster-id", "Bedrag", "Datum betaling", "Betalingskenmerk", "Screeningsorganisatie" };
			csvWriter.writeNext(header);

			dtos.forEach(verrichting ->
			{
				var line = new String[] { verrichting.getHuisartsLocatieNaam(), verrichting.getClientNaam(), verrichting.getMonsterId(), verrichting.getBedrag(),
					verrichting.getBetalingsdatum(), verrichting.getBetalingsKenmerk(), verrichting.getScreeningsorganisatie() };
				csvWriter.writeNext(line);
			});
		}

		return csvFile;
	}

	public Betaling convertFromDto(BetalingDto betalingDto)
	{
		Betaling betaling = new Betaling();
		betaling.setScreenitId(betalingDto.getScreenitId());
		betaling.setDebet(betalingDto.isDebet());
		betaling.setBetalingsKenmerk(betalingDto.getBetalingsKenmerk());
		betaling.setBetalingsdatum(betalingDto.getBetalingsdatum());
		betaling.setBedrag(betalingDto.getBedrag());
		return betaling;
	}

	public BetalingDto convertToDto(Betaling betaling, VerrichtingDto verrichtingDto)
	{
		BetalingDto betalingDto = new BetalingDto();
		betalingDto.setHuisartsportaalId(betaling.getHuisartsportaalId());
		betalingDto.setBedrag(betaling.getBedrag());
		betalingDto.setBetalingsKenmerk(betaling.getBetalingsKenmerk());
		betalingDto.setBedragString(NumberFormat.getCurrencyInstance().format(betaling.getBedrag()));
		betalingDto.setDebet(betaling.isDebet());
		betalingDto.setBetalingsKenmerk(betaling.getBetalingsKenmerk());
		betalingDto.setBetalingsdatum(betaling.getBetalingsdatum());
		betalingDto.setVerrichting(verrichtingDto);
		return betalingDto;
	}

	public BetalingDto convertToDisplayDto(Betaling betaling, Verrichting verrichting)
	{
		BetalingDto betalingDto = new BetalingDto();
		betalingDto.setHuisartsportaalId(betaling.getHuisartsportaalId());
		betalingDto.setBedrag(betaling.getBedrag());
		betalingDto.setBetalingsKenmerk(betaling.getBetalingsKenmerk());
		betalingDto.setBedragString(NumberFormat.getCurrencyInstance().format(betaling.getBedrag()));
		betalingDto.setDebet(betaling.isDebet());
		betalingDto.setBetalingsKenmerk(betaling.getBetalingsKenmerk());
		betalingDto.setBetalingsdatum(betaling.getBetalingsdatum());
		VerrichtingDto verrichtingDto = new VerrichtingDto();
		verrichtingDto.setClientNaam(verrichting.getClientNaam());
		verrichtingDto.setMonsterId(verrichting.getMonsterId());
		verrichtingDto.setRegio(verrichting.getRegio());
		verrichtingDto.setHuisartsLocatieNaam(verrichting.getHuisartsLocatie().getNaam());
		betalingDto.setVerrichting(verrichtingDto);
		return betalingDto;
	}

	private BetalingCsvDto convertToCsvDto(Betaling betaling)
	{
		BetalingCsvDto betalingCsvDto = new BetalingCsvDto();
		betalingCsvDto.setBedrag(betaling.getBedrag().toString());
		betalingCsvDto.setBetalingsdatum(verrichtingenService.getPrintDatum(betaling.getBetalingsdatum()));
		betalingCsvDto.setBetalingsKenmerk(betaling.getBetalingsKenmerk());
		betalingCsvDto.setClientNaam(betaling.getVerrichting().getClientNaam());
		betalingCsvDto.setHuisartsLocatieNaam(betaling.getVerrichting().getHuisartsLocatie().getNaam());
		betalingCsvDto.setMonsterId(betaling.getVerrichting().getMonsterId());
		betalingCsvDto.setScreeningsorganisatie(betaling.getVerrichting().getRegio());
		return betalingCsvDto;
	}

	private BigDecimal getTotaalBedrag(Huisarts huisarts, BetalingZoekObjectDto betalingZoekObjectDto)
	{
		BigDecimal totaalBedrag = betalingCriteriaRepository.getBetalingenTotaalBedrag(huisarts, betalingZoekObjectDto);
		if (totaalBedrag == null)
		{
			totaalBedrag = BigDecimal.ZERO;
		}
		return totaalBedrag;
	}
}

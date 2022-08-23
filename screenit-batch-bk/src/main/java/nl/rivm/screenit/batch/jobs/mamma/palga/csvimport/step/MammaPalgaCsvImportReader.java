package nl.rivm.screenit.batch.jobs.mamma.palga.csvimport.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.text.SimpleDateFormat;
import java.util.Arrays;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.BaseCsvFileReader;
import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.dto.mamma.MammaPalgaCsvImportDto;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.service.mamma.MammaPalgaCsvImportMapping;
import nl.rivm.screenit.service.mamma.MammaPalgaService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public class MammaPalgaCsvImportReader extends BaseCsvFileReader<MammaPalgaCsvImportDto>
{

	private MammaPalgaCsvImportMapping importMapping = null;

	private static final int ROW_LENGTH = 26;

	private static final int HEADER_ROW = 1;

	@Autowired
	private MammaPalgaService palgaService;

	@Override
	protected MammaPalgaCsvImportDto parseLine(String[] line, int regelnummer, String bestandsNaam) throws IllegalStateException
	{
		MammaPalgaCsvImportDto importDto = new MammaPalgaCsvImportDto();
		importDto.setRegelNummer(regelnummer);

		if (regelnummer == HEADER_ROW && Arrays.stream(line).anyMatch(l -> StringUtils.equalsIgnoreCase("PseudoID", l)))
		{
			importMapping = palgaService.maakImportDtoMapping(line);
		}
		else if (line.length == ROW_LENGTH)
		{
			try
			{
				SimpleDateFormat dateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);

				importDto.setPseudoId((Long.parseLong(line[importMapping.getPseudoId()])));
				importDto.setGeboortejaar(Integer.parseUnsignedInt(line[importMapping.getGeboortejaar()]));
				String aanVangVerrichting = line[importMapping.getAanvangVerrichting()];
				if (StringUtils.isNotBlank(aanVangVerrichting))
				{
					importDto.setAanvangVerrichting(dateFormat.parse(aanVangVerrichting));
				}
				importDto.setEindeVerrichting(dateFormat.parse(line[importMapping.getEindeVerrichting()]));
				importDto.setDatumOntvangstMateriaal(dateFormat.parse(line[importMapping.getDatumOntvangstMateriaal()]));
				importDto.setDatumEersteAutorisatie(dateFormat.parse(line[importMapping.getDatumEersteAutorisatie()]));
				importDto.setVerkrijgingswijze(line[importMapping.getVerkrijgingswijze()]);
				importDto.setZijdigheid(StringUtils.defaultIfBlank(line[importMapping.getZijdigheid()], null));
				importDto.setLocatie(StringUtils.defaultIfBlank(line[importMapping.getLocatie()], null));
				importDto.setLocatieInUren(StringUtils.defaultIfBlank(line[importMapping.getLocatieInUren()], null));
				importDto.setOestrogeenReceptorStatus(StringUtils.defaultIfBlank(line[importMapping.getOestrogeenReceptorStatus()], null));
				importDto.setProgesteronReceptorStatus(StringUtils.defaultIfBlank(line[importMapping.getProgesteronReceptorStatus()], null));
				importDto.setHer2Status(StringUtils.defaultIfBlank(line[importMapping.getHer2Status()], null));
				importDto.setBClassificatie(StringUtils.defaultIfBlank(line[importMapping.getBClassificatie()], null));
				importDto.setCClassificatie(StringUtils.defaultIfBlank(line[importMapping.getCClassificatie()], null));
				importDto.setMaligniteitsgraad(StringUtils.defaultIfBlank(line[importMapping.getMaligniteitsgraad()], null));
				importDto.setPt(StringUtils.defaultIfBlank(line[importMapping.getPt()], null));
				importDto.setPn(StringUtils.defaultIfBlank(line[importMapping.getPn()], null));
				importDto.setTypeInvasieveTumor(StringUtils.defaultIfBlank(line[importMapping.getTypeInvasieveTumor()], null));
				importDto.setGraderingDcis(StringUtils.defaultIfBlank(line[importMapping.getGraderingDcis()], null));
				importDto.setTypeNietEenduidigBenigneLaesies(StringUtils.defaultIfBlank(line[importMapping.getTypeNietEenduidigBenigneLaesies()], null));
				importDto.setTypeEenduidigBenigneLaesies(StringUtils.defaultIfBlank(line[importMapping.getTypeEenduidigBenigneLaesies()], null));
				importDto.setTypeCis(StringUtils.defaultIfBlank(line[importMapping.getTypeCis()], null));
				String versieProtocol = line[importMapping.getVersieProtocol()];
				importDto.setVersieProtocol(StringUtils.defaultIfBlank(versieProtocol, null));

				String reedAangeleverd = line[importMapping.getIsReedsAangeleverd()];
				if (reedAangeleverd.equals("1"))
				{
					importDto.setReedsAangeleverd(true);
				}
				else if (!reedAangeleverd.equals("0"))
				{
					throw new IllegalArgumentException("reedsAangeleverd moet 0 of 1 zijn");
				}

				String matchniveau = line[importMapping.getMatchniveau()];
				if (matchniveau.equals("1"))
				{
					importDto.setPatid3(true);
				}
				else if (!matchniveau.equals("0"))
				{
					throw new IllegalArgumentException("matchniveau moet 0 of 1 zijn");
				}

				LOG.trace("#" + regelnummer + ": ingelezen");
			}
			catch (Exception e)
			{
				logFout(e, importDto);
			}
		}
		else
		{
			logFout(null, importDto);
		}
		return importDto;
	}

	private void logFout(Exception e, MammaPalgaCsvImportDto dto)
	{
		dto.setFout(true);
		String errorMessage = "#" + dto.getRegelNummer() + ": technisch";

		String melding = (getExecutionContext().containsKey(BatchConstants.MELDING) ? getExecutionContext().getString(BatchConstants.MELDING) + errorMessage
			: "Er zijn een aantal fouten gevonden tijdens het uitvoeren van de import:<br>" + errorMessage) + "<br>";
		getExecutionContext().putString(BatchConstants.MELDING, melding);
		getExecutionContext().put(BatchConstants.LEVEL, Level.WARNING);
		if (e != null)
		{
			LOG.error(errorMessage, e);
		}
		else
		{
			LOG.error(errorMessage);
		}
	}

}

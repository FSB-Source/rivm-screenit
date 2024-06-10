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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.MammaDense2ConfiguratieDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameter;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDenseWaarde;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.mamma.MammaBaseDense2Service;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification;
import nl.rivm.screenit.specification.mamma.MammaMammografieSpecification;
import nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.CsvUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import au.com.bytecode.opencsv.CSVWriter;

@Slf4j
@Service
@AllArgsConstructor
public class MammaBaseDense2ServiceImpl implements MammaBaseDense2Service
{
	private final OrganisatieParameterService organisatieParameterService;

	private final ClientRepository clientRepository;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final FileService fileService;

	private final String locatieFilestore;

	private final ClientService clientService;

	@Override
	public String getExportDirectory()
	{
		return locatieFilestore + FileStoreLocation.MAMMA_DENSE2_CSV_EXPORT.getPath();
	}

	@Override
	public File getExport()
	{
		try
		{
			if (!fileService.exists(getExportDirectory()))
			{
				return null;
			}

			var exportFolderContent = fileService.listFilesGesorteerd(getExportDirectory());
			if (exportFolderContent.isEmpty())
			{
				LOG.warn("Geen export bestanden gevonden");
				return null;
			}
			return fileService.load(exportFolderContent.get(0));
		}
		catch (IOException ex)
		{
			LOG.error("Fout bij ophalen van export bestand", ex);
			return null;
		}
	}

	@Override
	public MammaDense2ConfiguratieDto getConfiguratie()
	{
		var configuratie = new MammaDense2ConfiguratieDto();
		var exportBestand = getExport();
		if (exportBestand != null)
		{
			configuratie.setExportBestandsnaam(exportBestand.getName());
		}
		configuratie.setCem(organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE2_CEM_PROJECT));
		configuratie.setMri(organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE2_MRI_PROJECT));
		configuratie.setControleGroep(organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE2_CONTROLEGROEP_PROJECT));
		configuratie.setHerinneringCem(organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE2_HERINNEREN_CEM_PROJECT));
		configuratie.setHerinneringMri(organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE2_HERINNEREN_MRI_PROJECT));
		configuratie.setMetingOpslaan(organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE_2_INITIELE_METING_OPSLAAN, true));
		String excludeProjecten = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE2_EXCLUDE_PROJECTEN);
		if (!StringUtils.isBlank(excludeProjecten))
		{
			var excludeProjectIds = Stream.of(excludeProjecten.split(",")).map(Long::parseLong).collect(Collectors.toList());
			configuratie.setExcludeProjecten(excludeProjectIds);
		}
		return configuratie;
	}

	@Override
	@Transactional
	public void updateConfiguratie(MammaDense2ConfiguratieDto configuratie, InstellingGebruiker instellingGebruiker)
	{
		var parameters = new ArrayList<OrganisatieParameter>();
		parameters.add(organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.MAMMA_DENSE2_CEM_PROJECT, configuratie.getCem(), null));
		parameters.add(organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.MAMMA_DENSE2_MRI_PROJECT, configuratie.getMri(), null));
		parameters.add(
			organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.MAMMA_DENSE2_CONTROLEGROEP_PROJECT, configuratie.getControleGroep(), null));
		parameters.add(organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.MAMMA_DENSE2_HERINNEREN_CEM_PROJECT, configuratie.getHerinneringCem(),
			null));
		parameters.add(organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.MAMMA_DENSE2_HERINNEREN_MRI_PROJECT, configuratie.getHerinneringMri(),
			null));
		parameters.add(
			organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.MAMMA_DENSE_2_INITIELE_METING_OPSLAAN,
				Boolean.toString(configuratie.isMetingOpslaan()), null));
		parameters.add(
			organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.MAMMA_DENSE2_EXCLUDE_PROJECTEN,
				StringUtils.join(configuratie.getExcludeProjecten(), ","), null));

		organisatieParameterService.saveOrUpdateOrganisatieParameters(parameters, instellingGebruiker);
	}

	public void genereerCsv(List<Client> clienten) throws IOException
	{
		leegExportsFolder();

		var fileName = "T1T7_" + currentDateSupplier.getLocalDateTime().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss")) + ".csv";
		var fullFilePath = getExportDirectory() + File.separator + fileName;
		var file = File.createTempFile(fileName, ".csv");
		try (var csvOutput = new CSVWriter(new FileWriter(file, false), ';', CSVWriter.NO_QUOTE_CHARACTER))
		{
			csvOutput.writeNext(new String[] { "PseudoID", "Postcode" });
			LOG.info("Start vullen van CSV voor download: {}, aantal clienten: {}", fileName, clienten.size());
			clienten.forEach(
				client -> csvOutput.writeNext(new String[] { Long.toString(client.getMammaDossier().getId()), clientService.getGbaPostcode(client).substring(0, 4) }));
		}
		CsvUtil.truncateLastLine(file);
		fileService.save(fullFilePath, file);
		fileService.delete(file.getPath());
	}

	@Override
	public List<Client> getExportClienten()
	{

		var minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name()) - 1;
		var maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name()) - 4;
		var minimaleOnderzoeksDatum = currentDateSupplier.getLocalDate().minusDays(27);
		var configuratie = getConfiguratie();
		var clienten = clientRepository.findAll(
			ClientSpecification.heeftActieveClientPredicate().toSpecification()
				.and(MammaMammografieSpecification.heeftClientLaatsteOnderzoekMetDensiteit(MammaDenseWaarde.D))
				.and(ClientSpecification.heeftGeboorteJaarVoorLeeftijdBereik(minimaleLeeftijd, maximaleLeeftijd, currentDateSupplier.getLocalDate()))
				.and(MammaOnderzoekSpecification.heeftClientOnderzoekAangemaaktVanaf(minimaleOnderzoeksDatum))
				.and(MammaBeoordelingSpecification.heeftClientLaatsteOnderzoekBeoordelingStatus(MammaBeoordelingStatus.UITSLAG_GUNSTIG))
				.and(MammaOnderzoekSpecification.heeftClientLaatsteOnderzoekVolledig()),
			Sort.by(Sort.Direction.DESC, "mammaDossier.laatsteScreeningRonde.laatsteOnderzoek.mammografie.afgerondOp")
		);
		return clienten.stream()
			.filter(c -> !heeftBezwaar(c) && !heeftClientAlMeegedaan(c, configuratie) && !zitClientInExclusieProjecten(c, configuratie))
			.collect(Collectors.toList());
	}

	private void leegExportsFolder()
	{
		try
		{
			fileService.cleanDirectory(getExportDirectory());
		}
		catch (IOException e)
		{
			LOG.error("Fout bij leegmaken van de exports folder", e);
		}
	}

	private boolean heeftBezwaar(Client client)
	{
		return BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK, Bevolkingsonderzoek.MAMMA)
			|| BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_KWALITEITSWAARBORGING, Bevolkingsonderzoek.MAMMA);
	}

	private boolean zitClientInExclusieProjecten(Client client, MammaDense2ConfiguratieDto configuratie)
	{
		return client.getProjecten().stream().anyMatch(p -> configuratie.getExcludeProjecten() != null && configuratie.getExcludeProjecten().contains(p.getProject().getId()));
	}

	private boolean heeftClientAlMeegedaan(Client client, MammaDense2ConfiguratieDto configuratie)
	{
		var projectClienten = ProjectUtil.getHuidigeProjectClienten(client, currentDateSupplier.getDate(), true);
		for (var projectClient : projectClienten)
		{
			if (configuratie.getDenseProjecten().stream().anyMatch(p -> p.equalsIgnoreCase(projectClient.getProject().getNaam())))
			{
				return true;
			}
		}

		return false;
	}
}

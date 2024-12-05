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
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.MammaDense2ConfiguratieDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameter;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDenseWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.repository.mamma.MammaMammografieRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.mamma.MammaBaseDense2Service;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.ProjectClientSpecification;
import nl.rivm.screenit.specification.algemeen.ProjectSpecification;
import nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification;
import nl.rivm.screenit.specification.mamma.MammaMammografieBaseSpecification;
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

import static nl.rivm.screenit.specification.SpecificationUtil.join;

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

	private final MammaMammografieRepository mammografieRepository;

	private LocalDate getMinimaleOnderzoekDatum()
	{
		return currentDateSupplier.getLocalDate().minusDays(27);
	}

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

	@Override
	public void genereerCsv(List<Client> clienten) throws IOException
	{
		leegExportsFolder();

		var fileName = "T1T7_" + currentDateSupplier.getLocalDateTime().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss")) + ".csv";
		var fullFilePath = getExportDirectory() + File.separator + fileName;
		var file = File.createTempFile(fileName, ".csv");
		schrijfClientenInCsv(clienten, fullFilePath, file, false);
	}

	@Override
	public void voegToeAanCsv(List<Client> clienten) throws IOException
	{
		var exportBestand = getExport();
		if (exportBestand == null)
		{
			LOG.info("Geen export bestand van eerste studieronde gevonden, nieuwe export wordt aangemaakt");
			genereerCsv(clienten);
			return;
		}

		var tempFilePath = Files.createTempFile(exportBestand.getName(), ".csv");
		var tempFile = tempFilePath.toFile();
		Files.copy(exportBestand.toPath(), tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		schrijfClientenInCsv(clienten, exportBestand.getPath(), tempFile, true);
	}

	private void schrijfClientenInCsv(List<Client> clienten, String exportBestandPad, File tijdelijkeExportBestand, boolean aanvullen) throws IOException
	{
		try (var csvOutput = new CSVWriter(new FileWriter(tijdelijkeExportBestand, aanvullen), ';', CSVWriter.NO_QUOTE_CHARACTER))
		{
			if (aanvullen)
			{

				csvOutput.writeNext(new String[] { "" });
			}
			else
			{

				csvOutput.writeNext(new String[] { "PseudoID", "Postcode" });
			}
			clienten.forEach(
				client -> csvOutput.writeNext(
					new String[] { Long.toString(client.getMammaDossier().getId()), clientService.getGbaPostcode(client).substring(0, 4) }));
		}
		CsvUtil.truncateLastLine(tijdelijkeExportBestand);
		fileService.save(exportBestandPad, tijdelijkeExportBestand);
		fileService.delete(tijdelijkeExportBestand.getPath());
	}

	@Override
	public List<Client> getExportClientenEersteStudieronde()
	{

		var minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name()) - 1;
		var maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name()) - 4;
		var configuratie = getConfiguratie();
		var clienten = clientRepository.findAll(
			ClientSpecification.heeftActieveClient()
				.and(MammaMammografieBaseSpecification.heeftDensiteit(MammaDenseWaarde.D).with(mammografieJoin()))
				.and(ClientSpecification.heeftGeboorteJaarVoorLeeftijdBereik(minimaleLeeftijd, maximaleLeeftijd, currentDateSupplier.getLocalDate()))
				.and(MammaOnderzoekSpecification.isAangemaaktVanaf(getMinimaleOnderzoekDatum()).with(r -> onderzoekJoin(r)))
				.and(MammaBeoordelingSpecification.heeftClientLaatsteOnderzoekBeoordelingStatus(MammaBeoordelingStatus.UITSLAG_GUNSTIG))
				.and(MammaOnderzoekSpecification.heeftStatus(MammaOnderzoekStatus.AFGEROND).with(r -> onderzoekJoin(r))),
			getSorteerVolgorde()
		);
		return clienten.stream()
			.filter(c -> !heeftBezwaar(c) && !clientZitInDense2Project(c, configuratie) && !zitClientInExclusieProjecten(c, configuratie))
			.collect(Collectors.toList());
	}

	@Override
	public boolean clientZitInDense2Project(Client client, MammaDense2ConfiguratieDto configuratie)
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

	@Override
	public List<Client> getExportClientenTweedeStudieronde()
	{
		var configuratie = getConfiguratie();
		return clientRepository.findAll(MammaOnderzoekSpecification.isAangemaaktVanaf(getMinimaleOnderzoekDatum()).with((From<?, ? extends Client> r) -> onderzoekJoin(r))
				.and(MammaBeoordelingSpecification.heeftStatus(MammaBeoordelingStatus.UITSLAG_GUNSTIG)
					.with((From<?, ? extends Client> r) -> join(onderzoekJoin(r), MammaOnderzoek_.laatsteBeoordeling))
					.and(ProjectSpecification.heeftNaamIn(configuratie.getDenseOnderzoekProjecten()).with(projectJoin()))
					.and(ProjectClientSpecification.clientIsToegevoegdVoorDatum(getMinimaleOnderzoekDatum()).with(r -> join(r, Client_.projecten)))),
			getSorteerVolgorde()
		);
	}

	@Override
	public ExtendedSpecification<Client> getClientenSpecificationVoorVerwijderenDensiteit()
	{
		var configuratie = getConfiguratie();
		var verwijderenVanaf = currentDateSupplier.getLocalDate().minusDays(30);

		return MammaBeoordelingSpecification.heeftStatusDatumOpOfVoor(verwijderenVanaf).with(beoordelingJoin())
			.and(MammaMammografieBaseSpecification.heeftDensiteit().with(mammografieJoin()))
			.and(MammaBeoordelingSpecification.heeftStatus(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG).with(beoordelingJoin())
				.or(MammaBeoordelingSpecification.heeftStatus(MammaBeoordelingStatus.UITSLAG_GUNSTIG).with(beoordelingJoin())
					.and(ProjectSpecification.heeftNietNaamIn(configuratie.getDenseOnderzoekProjecten()).with(projectJoin()))
				)
			);
	}

	private Join<MammaScreeningRonde, MammaOnderzoek> onderzoekJoin(From<?, ? extends Client> r)
	{
		var dossierJoin = join(r, Client_.mammaDossier);
		var rondeJoin = join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
		return join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
	}

	private Function<From<?, ? extends Client>, From<?, ? extends MammaMammografie>> mammografieJoin()
	{
		return r ->
		{
			var onderzoekJoin = onderzoekJoin(r);
			return join(onderzoekJoin, MammaOnderzoek_.mammografie);
		};
	}

	private Function<From<?, ? extends Client>, From<?, ? extends MammaBeoordeling>> beoordelingJoin()
	{
		return r ->
		{
			var onderzoekJoin = onderzoekJoin(r);
			return join(onderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);
		};
	}

	private Function<From<?, ? extends Client>, From<?, ? extends Project>> projectJoin()
	{
		return r ->
		{
			var projectClientJoin = join(r, Client_.projecten, JoinType.LEFT);
			return join(projectClientJoin, ProjectClient_.project, JoinType.LEFT);
		};
	}

	@Override
	public boolean magDensiteitOpslaan(MammaDenseWaarde densiteit, Client client)
	{
		var metingOpslaan = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE_2_INITIELE_METING_OPSLAAN, false);
		var clientInDense2Project = clientZitInDense2Project(client, getConfiguratie());
		var hogeDensiteit = densiteit == MammaDenseWaarde.D;
		if (clientInDense2Project)
		{
			return false;
		}
		else
		{
			return metingOpslaan && hogeDensiteit;
		}
	}

	@Override
	@Transactional
	public void verwijderDensiteit(MammaOnderzoek onderzoek)
	{
		var mammografie = onderzoek.getMammografie();
		if (mammografie == null)
		{
			return;
		}
		mammografie.setDensiteit(null);
		mammografieRepository.save(mammografie);
	}

	private Sort getSorteerVolgorde()
	{
		return Sort.by(Sort.Direction.DESC, "mammaDossier.laatsteScreeningRonde.laatsteOnderzoek.creatieDatum");
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

}

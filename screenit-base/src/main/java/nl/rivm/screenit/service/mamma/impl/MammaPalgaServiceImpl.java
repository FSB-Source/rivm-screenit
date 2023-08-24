package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import net.lingala.zip4j.exception.ZipException;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.VerslagDao;
import nl.rivm.screenit.dao.mamma.MammaPalgaDao;
import nl.rivm.screenit.dto.mamma.MammaPalgaCsvImportDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportConfig;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaGrondslag;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpFollowupPa;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpMonstermateriaal;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpPathologieMedischeObservatie;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpPtnmEnGradering;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerrichting;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaPalgaCsvImportMapping;
import nl.rivm.screenit.service.mamma.MammaPalgaService;
import nl.rivm.screenit.service.mamma.MammaVerwerkVerslagService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ZipUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaPalgaServiceImpl implements MammaPalgaService
{
	@Autowired
	private String locatieFilestore;

	@Autowired
	private MammaPalgaDao palgaDao;

	@Autowired
	private LogService logService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private VerslagDao verslagDao;

	@Autowired
	private MammaVerwerkVerslagService verwerkVerslagService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Long> getClientenVoorPalga(MammaPalgaExportConfig exportConfig)
	{
		return palgaDao.getClientenVoorPalga(exportConfig);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteExports(String naam, Account loggedInAccount)
	{
		if (naam != null && loggedInAccount != null)
		{
			String logRegel = String.format("Verwijderd: %s", naam);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_PALGA_CSV_EXPORT, loggedInAccount, logRegel);
		}
		List<UploadDocument> exports = palgaDao.getExports();
		for (UploadDocument export : exports)
		{
			uploadDocumentService.delete(export);
		}
	}

	@Override
	public UploadDocument getExport()
	{
		return palgaDao.getExport();
	}

	@Override
	public UploadDocument getImport()
	{
		return palgaDao.getImport();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateExport(UploadDocument zipDocument) throws IOException
	{
		uploadDocumentService.saveOrUpdate(zipDocument, FileStoreLocation.MAMMA_PALGA_CSV_EXPORT, null, true);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateImport(UploadDocument importDocument) throws IOException
	{
		if (importDocument != getImport())
		{
			deleteImports();
			uploadDocumentService.saveOrUpdate(importDocument, FileStoreLocation.MAMMA_PALGA_CSV_IMPORT, null, true);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public String saveOrUpdateImportZip(UploadDocument importDocument, String wachtwoord)
	{
		try
		{
			saveOrUpdateImport(extractImport(importDocument, wachtwoord));
		}
		catch (ZipException zipException)
		{
			LOG.error("Fout bij unzippen", zipException);
			if (zipException.getType().name().equals("WRONG_PASSWORD"))
			{
				return "Incorrect wachtwoord";
			}
			else
			{
				return zipException.getMessage();
			}
		}
		catch (Exception e)
		{
			LOG.error("Onbekende fout", e);
			return e.getMessage();
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteImports()
	{
		List<UploadDocument> imports = palgaDao.getImports();
		for (UploadDocument importDoc : imports)
		{
			uploadDocumentService.delete(importDoc);
		}
	}

	private UploadDocument extractImport(UploadDocument importDocument, String zipWachtwoord) throws ZipException
	{
		List<File> files = ZipUtil.extractZip(importDocument.getFile(), zipWachtwoord,
			locatieFilestore + File.separator + FileStoreLocation.MAMMA_PALGA_CSV_IMPORT.getPath(),
			true);
		if (files.size() != 1 || !files.get(0).getName().endsWith(".csv"))
		{
			throw new IllegalArgumentException("Zip moet exact 1 csv bestand bevatten");
		}
		else
		{
			return new UploadDocument(files.get(0), null, "application/octet-stream", true);
		}
	}

	@Override
	public MammaPalgaCsvImportMapping maakImportDtoMapping(String[] row)
	{
		LOG.debug("Begonnen met aanmaken mappings object.");
		MammaPalgaCsvImportMapping mapping = new MammaPalgaCsvImportMapping();
		int column = 0;
		for (String headerText : row)
		{
			if (StringUtils.isBlank(headerText))
			{
				throw new IllegalArgumentException("Kolom " + (column + 1) + " heeft lege kolomheader");
			}
			vulMappingVoorKolom(mapping, headerText, column);
			column++;
		}
		LOG.debug("Klaar met aanmaken mappings object.");
		return mapping;
	}

	private void vulMappingVoorKolom(MammaPalgaCsvImportMapping mapping, String headerTekst, int column)
	{
		switch (headerTekst.toLowerCase())
		{
		case "pseudoid":
			mapping.setPseudoId(column);
			break;
		case "geboortejaar":
			mapping.setGeboortejaar(column);
			break;
		case "aanvang_verrichting":
			mapping.setAanvangVerrichting(column);
			break;
		case "einde_verrichting":
			mapping.setEindeVerrichting(column);
			break;
		case "datum_ontvangst_materiaal":
			mapping.setDatumOntvangstMateriaal(column);
			break;
		case "datum_eerste_autorisatie":
			mapping.setDatumEersteAutorisatie(column);
			break;
		case "verkrijgingswijze":
			mapping.setVerkrijgingswijze(column);
			break;
		case "zijdigheid":
			mapping.setZijdigheid(column);
			break;
		case "locatie_topografie":
			mapping.setLocatie(column);
			break;
		case "locatie_in_uren":
			mapping.setLocatieInUren(column);
			break;
		case "oestrogeen_receptor_status":
			mapping.setOestrogeenReceptorStatus(column);
			break;
		case "progesteron_receptor_status":
			mapping.setProgesteronReceptorStatus(column);
			break;
		case "her2_status":
			mapping.setHer2Status(column);
			break;
		case "b_classificatie":
			mapping.setBClassificatie(column);
			break;
		case "c_classificatie":
			mapping.setCClassificatie(column);
			break;
		case "maligniteitsgraad":
			mapping.setMaligniteitsgraad(column);
			break;
		case "pt":
			mapping.setPt(column);
			break;
		case "pn":
			mapping.setPn(column);
			break;
		case "type_invasieve_tumor_overige":
			mapping.setTypeInvasieveTumor(column);
			break;
		case "gradering_dcis":
			mapping.setGraderingDcis(column);
			break;
		case "type_niet_eenduidig_benigne_laes":
			mapping.setTypeNietEenduidigBenigneLaesies(column);
			break;
		case "type_eenduidig_benigne_laesies":
			mapping.setTypeEenduidigBenigneLaesies(column);
			break;
		case "type_cis":
			mapping.setTypeCis(column);
			break;
		case "versie_protocol":
			mapping.setVersieProtocol(column);
			break;
		case "reeds_aangeleverd":
			mapping.setIsReedsAangeleverd(column);
			break;
		case "matchniveau":
			mapping.setMatchniveau(column);
			break;
		default:
			throw new IllegalArgumentException("Ongeldige kolomnaam: " + headerTekst);
		}
	}

	private DSValue getDsValue(String code, String varName, Class<?> clazz) throws NoSuchFieldException
	{
		if (code == null)
		{
			return null;
		}
		else if ("UNK".equals(code) && ("zijdigheid".equals(varName) || "bclassificatieOpMammabiopt".equals(varName)))
		{
			return verslagDao.getDsValue(code, "2.16.840.1.113883.5.1008", Constants.CDA_NULL_FLAVOR_VALUESET_NAME);
		}
		DSValueSet dsValueSet = clazz.getDeclaredField(varName).getAnnotation(DSValueSet.class);
		for (DSValueSetValue dsValue : dsValueSet.values())
		{
			if (dsValue.code().equals(code))
			{
				String codeSystem = dsValue.codeSystem();
				return verslagDao.getDsValue(code, codeSystem, dsValueSet.name(), false);
			}
		}
		throw new IllegalArgumentException(clazz.getSimpleName() + ":" + varName + ", " + code + " not found");
	}

	private long getPatid3MatchCount(GbaPersoon persoon)
	{
		return palgaDao.getPatid3MatchCount(persoon);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
	public String verwerkImportDto(MammaPalgaCsvImportDto dto, MammaPalgaGrondslag grondslag) throws NoSuchFieldException
	{
		MammaDossier dossier = hibernateService.get(MammaDossier.class, dto.getPseudoId());
		if (dossier == null)
		{
			return "pseudoId";
		}
		GbaPersoon persoon = dossier.getClient().getPersoon();
		if (dto.getGeboortejaar() != DateUtil.toLocalDate(persoon.getGeboortedatum()).getYear())
		{
			return "geboortejaar";
		}
		if (dto.isPatid3() && getPatid3MatchCount(persoon) != 1)
		{
			return "PATID3 (bsn: " + persoon.getBsn() + ")";
		}
		if (heeftBezwaar(dossier.getClient()))
		{
			return "bezwaar";
		}

		MammaScreeningRonde screeningsRonde = verwerkVerslagService.getValideScreeningsRonde(dossier.getClient(), dto.getEindeVerrichting());

		MammaFollowUpVerslag verslag = new MammaFollowUpVerslag();
		verslag.setScreeningRonde(screeningsRonde);
		MammaFollowUpVerslagContent verslagContent = new MammaFollowUpVerslagContent();
		verslagContent.setVerslag(verslag);

		MammaFollowUpFollowupPa followupPa = new MammaFollowUpFollowupPa();
		followupPa.setVerslagContent(verslagContent);

		MammaFollowUpMonstermateriaal monstermateriaal = new MammaFollowUpMonstermateriaal();
		monstermateriaal.setFollowupPa(followupPa);
		monstermateriaal.setLocatietopologie(getDsValue(dto.getLocatie(), "locatietopologie", MammaFollowUpMonstermateriaal.class));
		monstermateriaal.setLocatieuren(getDsValue(dto.getLocatieInUren(), "locatieuren", MammaFollowUpMonstermateriaal.class));
		monstermateriaal.setVerkrijgingswijze(getDsValue(dto.getVerkrijgingswijze(), "verkrijgingswijze", MammaFollowUpMonstermateriaal.class));
		monstermateriaal.setZijdigheid(getDsValue(dto.getZijdigheid(), "zijdigheid", MammaFollowUpMonstermateriaal.class));
		followupPa.setMonstermateriaal(monstermateriaal);
		followupPa.setCclassificatiePunctie(getDsValue(dto.getCClassificatie(), "cclassificatiePunctie", MammaFollowUpFollowupPa.class));
		followupPa.setOestrogeenReceptorStatus(getDsValue(dto.getOestrogeenReceptorStatus(), "oestrogeenReceptorStatus", MammaFollowUpFollowupPa.class));
		followupPa.setProgesteronReceptorStatus(getDsValue(dto.getProgesteronReceptorStatus(), "progesteronReceptorStatus", MammaFollowUpFollowupPa.class));
		followupPa.setHer2Status(getDsValue(dto.getHer2Status(), "her2Status", MammaFollowUpFollowupPa.class));
		followupPa.setBclassificatieOpMammabiopt(getDsValue(dto.getBClassificatie(), "bclassificatieOpMammabiopt", MammaFollowUpFollowupPa.class));
		followupPa.setMaligniteitsgraad(getDsValue(dto.getMaligniteitsgraad(), "maligniteitsgraad", MammaFollowUpFollowupPa.class));
		followupPa.setTypeInvasieveTumorwhoOverige(getDsValue(dto.getTypeInvasieveTumor(), "typeInvasieveTumorwhoOverige", MammaFollowUpFollowupPa.class));
		followupPa.setGraderingDcis(getDsValue(dto.getGraderingDcis(), "graderingDcis", MammaFollowUpFollowupPa.class));
		vulMeerkeuzeVeldPa(followupPa.getTypeNietEenduidigBenigneLaesies(), dto.getTypeNietEenduidigBenigneLaesies(), "typeNietEenduidigBenigneLaesies");
		vulMeerkeuzeVeldPa(followupPa.getTypeEenduidigBenigneLaesies(), dto.getTypeEenduidigBenigneLaesies(), "typeEenduidigBenigneLaesies");
		vulMeerkeuzeVeldPa(followupPa.getTypeCis(), dto.getTypeCis(), "typeCis");

		MammaFollowUpPtnmEnGradering ptnmEnGradering = new MammaFollowUpPtnmEnGradering();
		ptnmEnGradering.setFollowupPa(followupPa);
		ptnmEnGradering.setPt(getDsValue(dto.getPt(), "pt", MammaFollowUpPtnmEnGradering.class));
		ptnmEnGradering.setPn(getDsValue(dto.getPn(), "pn", MammaFollowUpPtnmEnGradering.class));
		followupPa.setPtnmEnGradering(ptnmEnGradering);
		verslagContent.setFollowupPa(Collections.singletonList(followupPa));

		MammaFollowUpPathologieMedischeObservatie medischeObservatie = new MammaFollowUpPathologieMedischeObservatie();
		medischeObservatie.setVerslagContent(verslagContent);
		medischeObservatie.setDatumAutorisatieUitslag(dto.getDatumEersteAutorisatie());
		medischeObservatie.setDatumOntvangstMateriaal(dto.getDatumOntvangstMateriaal());
		medischeObservatie.setVersieProtocol(dto.getVersieProtocol());
		medischeObservatie.setTnummerLaboratorium(Constants.BK_TNUMMER_ELEKTRONISCH);
		verslagContent.setPathologieMedischeObservatie(medischeObservatie);

		MammaFollowUpVerrichting verrichting = new MammaFollowUpVerrichting();
		verrichting.setAanvangVerrichting(dto.getAanvangVerrichting());
		verrichting.setEindeVerrichting(dto.getEindeVerrichting());
		verrichting.setVerslagContent(verslagContent);
		verslagContent.setVerrichting(verrichting);

		verslag.setVerslagContent(verslagContent);

		verslag.setStatus(VerslagStatus.AFGEROND);
		verslag.setType(grondslag.getVerslagType());
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());

		String validatieFout = valideerVerslag(verslag, dto.getVersieProtocol() != null);
		if (validatieFout != null)
		{
			return "semantisch " + validatieFout;
		}

		verwerkVerslagService.onAfterVerwerkVerslagContent(verslag);
		verwerkVerslagService.verwerkImportVerslagInDossier(verslag);

		return null;
	}

	private void vulMeerkeuzeVeldPa(List<DSValue> targetList, String dtoTeksten, String varName) throws NoSuchFieldException
	{
		if (dtoTeksten == null)
		{
			return;
		}

		for (String optie : dtoTeksten.split("\\|"))
		{
			targetList.add(getDsValue(optie, varName, MammaFollowUpFollowupPa.class));
		}

		if (targetList.size() != targetList.stream().distinct().count())
		{
			throw new IllegalArgumentException("Dubbele waarde in meerkeuzeveld " + varName);
		}
	}

	private boolean heeftBezwaar(Client client)
	{
		return palgaDao.heeftBezwaar(client);
	}

	private String valideerVerslag(MammaFollowUpVerslag verslag, boolean isProtocol)
	{
		MammaFollowUpVerslagContent verslagContent = verslag.getVerslagContent();
		if (verslag.getScreeningRonde() == null)
		{
			return "screeningsronde";
		}
		if (!isValideMedischeObservatie(verslagContent.getPathologieMedischeObservatie()))
		{
			return "medische observatie";
		}
		if (!isValideVerrichting(verslagContent.getVerrichting()))
		{
			return "verrichting";
		}
		return valideerFollowUpPa(verslagContent.getFollowupPa().get(0), isProtocol);
	}

	private boolean isValideVerrichting(MammaFollowUpVerrichting verrichting)
	{
		return verrichting.getEindeVerrichting() != null && verrichting.getAanvangVerrichting() != null;
	}

	private boolean isValideMedischeObservatie(MammaFollowUpPathologieMedischeObservatie medischeObservatie)
	{
		return medischeObservatie.getTnummerLaboratorium() != null &&
			medischeObservatie.getDatumAutorisatieUitslag() != null &&
			medischeObservatie.getDatumOntvangstMateriaal() != null;
	}

	private boolean ptnmIsNull(MammaFollowUpPtnmEnGradering ptnmEnGradering)
	{
		return ptnmEnGradering == null ||
			(ptnmEnGradering.getPtnmbreastGradering() == null &&
				ptnmEnGradering.getPt() == null &&
				ptnmEnGradering.getPn() == null &&
				ptnmEnGradering.getPm() == null);
	}

	private String valideerFollowUpPa(MammaFollowUpFollowupPa followupPa, boolean isProtocol)
	{
		MammaFollowUpMonstermateriaal monstermateriaal = followupPa.getMonstermateriaal();
		DSValue verkrijgingswijze = monstermateriaal.getVerkrijgingswijze();

		MammaFollowUpPtnmEnGradering ptnmEnGradering = followupPa.getPtnmEnGradering();

		if (verkrijgingswijze == null)
		{
			return "verkrijgingswijze leeg";
		}

		if (monstermateriaal.getZijdigheid() == null ||
			monstermateriaal.getLocatieuren() != null == (monstermateriaal.getLocatietopologie() != null) && isProtocol)
		{
			return "monstermateriaal incompleet";
		}

		switch (verkrijgingswijze.getCode())
		{
		case "129300006": 
			return !punctieValide(followupPa, ptnmEnGradering) ? "punctie" : null;

		case "129249002": 
			return !bioptValide(followupPa, ptnmEnGradering) ? "biopt" : null;

		case "65801008": 
			return !excisieValide(followupPa) ? "excisie" : null;
		default:
			return "verkrijginswijze code";
		}
	}

	private boolean punctieValide(MammaFollowUpFollowupPa followupPa, MammaFollowUpPtnmEnGradering ptnmEnGradering)
	{
		return followupPa.getOestrogeenReceptorStatus() == null &&
			followupPa.getProgesteronReceptorStatus() == null &&
			followupPa.getHer2Status() == null &&
			followupPa.getBclassificatieOpMammabiopt() == null &&
			ptnmIsNull(ptnmEnGradering);
	}

	private boolean bioptValide(MammaFollowUpFollowupPa followupPa, MammaFollowUpPtnmEnGradering ptnmEnGradering)
	{
		return followupPa.getCclassificatiePunctie() == null && ptnmIsNull(ptnmEnGradering);

	}

	private boolean excisieValide(MammaFollowUpFollowupPa followupPa)
	{
		return followupPa.getCclassificatiePunctie() == null && followupPa.getBclassificatieOpMammabiopt() == null;
	}
}

package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.lingala.zip4j.exception.ZipException;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.VerslagDao;
import nl.rivm.screenit.dao.mamma.MammaPalgaDao;
import nl.rivm.screenit.dto.mamma.MammaPalgaCsvImportDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
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
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaPalgaCsvImportMapping;
import nl.rivm.screenit.service.mamma.MammaPalgaService;
import nl.rivm.screenit.service.mamma.MammaVerwerkVerslagService;
import nl.rivm.screenit.util.ZipUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaPalgaServiceImpl implements MammaPalgaService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaPalgaService.class);

	@Autowired
	private String locatieFilestore;

	@Autowired
	private MammaPalgaDao palgaDao;

	@Autowired
	private LogService logService;

	@Autowired
	private FileService fileService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private VerslagDao verslagDao;

	@Autowired
	private MammaVerwerkVerslagService verwerkVerslagService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Long> getClientenVoorPalga()
	{
		return palgaDao.getClientenVoorPalga();
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
		palgaDao.deleteExports();
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
		fileService.saveOrUpdateUploadDocument(zipDocument, FileStoreLocation.MAMMA_PALGA_CSV_EXPORT, null, true);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateImport(UploadDocument importDocument) throws IOException
	{
		if (importDocument != getImport())
		{
			deleteImports();
			fileService.saveOrUpdateUploadDocument(importDocument, FileStoreLocation.MAMMA_PALGA_CSV_IMPORT, null, true);
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
			LOG.error("ongebekende fout", e);
			return e.getMessage();
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteImports()
	{
		palgaDao.deleteImports();
	}

	private UploadDocument extractImport(UploadDocument importDocument, String zipWachtwoord) throws ZipException
	{
		List<File> files = ZipUtil.extractZip(importDocument.getFile(), zipWachtwoord, locatieFilestore + File.separator + FileStoreLocation.MAMMA_PALGA_CSV_IMPORT.getPath(),
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
		for (String mappingString : row)
		{
			if (StringUtils.isNotBlank(mappingString))
			{
				if (StringUtils.equalsIgnoreCase("PseudoID", mappingString))
				{
					mapping.setPseudoId(column);
				}
				else if (StringUtils.equalsIgnoreCase("Geboortejaar", mappingString))
				{
					mapping.setGeboortejaar(column);
				}
				else if (StringUtils.equalsIgnoreCase("aanvang_verrichting", mappingString))
				{
					mapping.setAanvangVerrichting(column);
				}
				else if (StringUtils.equalsIgnoreCase("einde_verrichting", mappingString))
				{
					mapping.setEindeVerrichting(column);
				}
				else if (StringUtils.equalsIgnoreCase("datum_ontvangst_materiaal", mappingString))
				{
					mapping.setDatumOntvangstMateriaal(column);
				}
				else if (StringUtils.equalsIgnoreCase("datum_eerste_autorisatie", mappingString))
				{
					mapping.setDatumEersteAutorisatie(column);
				}
				else if (StringUtils.equalsIgnoreCase("Verkrijgingswijze", mappingString))
				{
					mapping.setVerkrijgingswijze(column);
				}
				else if (StringUtils.equalsIgnoreCase("Zijdigheid", mappingString))
				{
					mapping.setZijdigheid(column);
				}
				else if (StringUtils.equalsIgnoreCase("Locatie_topografie", mappingString))
				{
					mapping.setLocatie(column);
				}
				else if (StringUtils.equalsIgnoreCase("Locatie_in_uren", mappingString))
				{
					mapping.setLocatieInUren(column);
				}
				else if (StringUtils.equalsIgnoreCase("Oestrogeen_receptor_status", mappingString))
				{
					mapping.setOestrogeenReceptorStatus(column);
				}
				else if (StringUtils.equalsIgnoreCase("Progesteron_receptor_status", mappingString))
				{
					mapping.setProgesteronReceptorStatus(column);
				}
				else if (StringUtils.equalsIgnoreCase("HER2_status", mappingString))
				{
					mapping.setHer2Status(column);
				}
				else if (StringUtils.equalsIgnoreCase("B_classificatie", mappingString))
				{
					mapping.setbClassificatie(column);
				}
				else if (StringUtils.equalsIgnoreCase("C_classificatie", mappingString))
				{
					mapping.setcClassificatie(column);
				}
				else if (StringUtils.equalsIgnoreCase("Maligniteitsgraad", mappingString))
				{
					mapping.setMaligniteitsgraad(column);
				}
				else if (StringUtils.equalsIgnoreCase("pTNM", mappingString))
				{
					mapping.setPtnm(column);
				}
				else if (StringUtils.equalsIgnoreCase("Stadiering", mappingString))
				{
					mapping.setStadiering(column);
				}
				else if (StringUtils.equalsIgnoreCase("Versie_protocol", mappingString))
				{
					mapping.setVersieProtocol(column);
				}
				else if (StringUtils.equalsIgnoreCase("Reeds_aangeleverd", mappingString))
				{
					mapping.setIsReedsAangeleverd(column);
				}
				else if (StringUtils.equalsIgnoreCase("Matchniveau", mappingString))
				{
					mapping.setMatchniveau(column);
				}
			}
			column++;
		}
		LOG.debug("Klaar met aanmaken mappings object.");
		return mapping;
	}

	private DSValue getDsValue(String code, String varName, Class clazz, boolean ignoreCase) throws NoSuchFieldException
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
			if ((ignoreCase ? dsValue.code().toLowerCase() : dsValue.code()).equals(ignoreCase ? code.toLowerCase() : code))
			{
				String codeSystem = dsValue.codeSystem();
				return verslagDao.getDsValue(code, codeSystem, dsValueSet.name(), ignoreCase);
			}
		}
		throw new IllegalArgumentException(clazz.getSimpleName() + ":" + varName + ", " + code + " not found");
	}

	private DSValue getDsValue(String code, String varName, Class clazz) throws NoSuchFieldException
	{
		return getDsValue(code, varName, clazz, false);
	}

	private String getPtString(String code)
	{
		if (code != null)
		{
			Pattern pattern = Pattern.compile("(?<=[Pp])(.*?)(?=[Nn(])");
			Matcher matcher = pattern.matcher(code);
			if (matcher.find())
			{
				code = matcher.group(1);
			}
		}
		return code;
	}

	private String getPnString(String code)
	{
		if (code != null)
		{
			Pattern pattern = Pattern.compile("([Nn].*?)(?=\\(|$)");
			Matcher matcher = pattern.matcher(code);
			if (matcher.find())
			{
				code = matcher.group(1);
			}
		}
		return code;
	}

	private long getPatid3MatchCount(GbaPersoon persoon)
	{
		return palgaDao.getPatid3MatchCount(persoon);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED, noRollbackFor = IllegalArgumentException.class)
	public String verwerkImportDto(MammaPalgaCsvImportDto dto) throws NoSuchFieldException
	{
		MammaDossier dossier = hibernateService.get(MammaDossier.class, dto.getPseudoId());
		GbaPersoon persoon = dossier.getClient().getPersoon();
		if (!DateUtils.truncate(persoon.getGeboortedatum(), Calendar.YEAR).equals(dto.getGeboortejaar()))
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
		followupPa.setCclassificatiePunctie(getDsValue(dto.getcClassificatie(), "cclassificatiePunctie", MammaFollowUpFollowupPa.class));
		followupPa.setOestrogeenReceptorStatus(getDsValue(dto.getOestrogeenReceptorStatus(), "oestrogeenReceptorStatus", MammaFollowUpFollowupPa.class));
		followupPa.setProgesteronReceptorStatus(getDsValue(dto.getProgesteronReceptorStatus(), "progesteronReceptorStatus", MammaFollowUpFollowupPa.class));
		followupPa.setHer2Status(getDsValue(dto.getHer2Status(), "her2Status", MammaFollowUpFollowupPa.class));
		followupPa.setBclassificatieOpMammabiopt(getDsValue(dto.getbClassificatie(), "bclassificatieOpMammabiopt", MammaFollowUpFollowupPa.class));
		followupPa.setMaligniteitsgraad(getDsValue(dto.getMaligniteitsgraad(), "maligniteitsgraad", MammaFollowUpFollowupPa.class));

		String ptnmCode = dto.getPtnm();
		if (dto.getPtnm() != null && dto.getPtnm().charAt(0) != 'y')
		{
			MammaFollowUpPtnmEnGradering ptnmEnGradering = new MammaFollowUpPtnmEnGradering();
			ptnmEnGradering.setFollowupPa(followupPa);
			ptnmEnGradering.setPtnmbreastGradering(getDsValue(dto.getStadiering(), "ptnmbreastGradering", MammaFollowUpPtnmEnGradering.class));
			boolean isAsku = ptnmCode.contains("ASKU");
			String ptString = isAsku ? "ASKU" : getPtString(ptnmCode);
			ptnmEnGradering.setPt(getDsValue(ptString, "pt", MammaFollowUpPtnmEnGradering.class, true));
			String pnString = isAsku ? "ASKU" : getPnString(ptnmCode);
			ptnmEnGradering.setPn(getDsValue(pnString, "pn", MammaFollowUpPtnmEnGradering.class, true));
			ptnmEnGradering.setPm(getDsValue("ASKU", "pm", MammaFollowUpPtnmEnGradering.class));
			followupPa.setPtnmEnGradering(ptnmEnGradering);
		}
		verslagContent.setFollowupPa(Arrays.asList(followupPa));

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
		verslag.setType(VerslagType.MAMMA_PA_FOLLOW_UP);
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());

		String validatieFout = valideerVerslag(verslag, dto.getVersieProtocol() != null, ptnmCode != null && ptnmCode.charAt(0) == 'y');
		if (validatieFout != null)
		{
			return "semantisch " + validatieFout;
		}

		verwerkVerslagService.onAfterVerwerkVerslagContent(verslag);
		verwerkVerslagService.verwerkImportVerslagInDossier(verslag);

		return null;
	}

	private boolean heeftBezwaar(Client client)
	{
		return palgaDao.heeftBezwaar(client);
	}

	private String valideerVerslag(MammaFollowUpVerslag verslag, boolean isProtocol, boolean skipPtnm)
	{
		MammaFollowUpVerslagContent verslagContent = verslag.getVerslagContent();
		return verslag.getScreeningRonde() == null ? "screeningsronde"
			: !isValideMedischeObservatie(verslagContent.getPathologieMedischeObservatie()) ? "medische observatie"
				: !isValideVerrichting(verslagContent.getVerrichting()) ? "verrichting"
					: valideerFollowUpPa(verslagContent.getFollowupPa().get(0), isProtocol, skipPtnm);
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

	private String valideerFollowUpPa(MammaFollowUpFollowupPa followupPa, boolean isProtocol, boolean skipPtnm)
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
			return !(followupPa.getOestrogeenReceptorStatus() == null &&
				followupPa.getProgesteronReceptorStatus() == null &&
				followupPa.getHer2Status() == null &&
				followupPa.getBclassificatieOpMammabiopt() == null &&
				ptnmEnGradering == null &&
				(followupPa.getCclassificatiePunctie() != null || !isProtocol)) ? "punctie" : null;

		case "129249002": 
			return !(followupPa.getCclassificatiePunctie() == null &&
				ptnmEnGradering == null &&
				(followupPa.getOestrogeenReceptorStatus() != null &&
					followupPa.getProgesteronReceptorStatus() != null &&
					followupPa.getHer2Status() != null
					&& followupPa.getBclassificatieOpMammabiopt() != null ||
					!isProtocol)) ? "biopt" : null;

		case "65801008": 
			return !(followupPa.getCclassificatiePunctie() == null &&
				followupPa.getBclassificatieOpMammabiopt() == null &&
				(followupPa.getOestrogeenReceptorStatus() != null &&
					followupPa.getProgesteronReceptorStatus() != null &&
					followupPa.getHer2Status() != null &&
					(skipPtnm && ptnmEnGradering == null ||
						ptnmEnGradering != null &&
							ptnmEnGradering.getPt() != null &&
							ptnmEnGradering.getPn() != null &&
							ptnmEnGradering.getPm() != null &&
							ptnmEnGradering.getPtnmbreastGradering() != null)
					|| !isProtocol)) ? "excisie" : null;
		default:
			return "verkrijginswijze code";
		}
	}
}

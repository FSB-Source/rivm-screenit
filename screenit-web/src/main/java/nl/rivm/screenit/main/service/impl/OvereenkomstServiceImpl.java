
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.text.SimpleDateFormat;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.dao.OvereenkomstDao;
import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsSyncService;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken.OvereenkomstZoekFilter;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class OvereenkomstServiceImpl implements OvereenkomstService
{

	private static final Logger LOG = LoggerFactory.getLogger(OvereenkomstServiceImpl.class);

	@Autowired
	private FileService fileService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private OvereenkomstDao overeenkomstDao;

	@Autowired
	private LogService logService;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private CervixHuisartsSyncService cervixHuisartsSyncService;

	@Autowired
	@Qualifier(value = "applicationUrl")
	private String applicationUrl;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateOvereenkomst(Overeenkomst overeenkomst, FileUpload fileUpload, Account account)
	{
		if (OvereenkomstType.ZAKELIJKE_OVEREENKOMST == overeenkomst.getOvereenkomst())
		{
			overeenkomst.setOrganisatieType(OrganisatieType.HUISARTS);
		}

		boolean nieuwUploadDocument = false;
		if (overeenkomst.getDocument() == null)
		{
			nieuwUploadDocument = true;
			overeenkomst.setDocument(new UploadDocument());
			overeenkomst.getDocument().setActief(Boolean.TRUE);
		}

		if (fileUpload != null)
		{
			File tempFile = null;
			try
			{
				tempFile = fileUpload.writeToTempFile();
				overeenkomst.setLaatsteUpdateDocument(currentDateSupplier.getDate());
				overeenkomst.getDocument().setContentType(fileUpload.getContentType());
				overeenkomst.getDocument().setNaam(fileUpload.getClientFileName());
				overeenkomst.getDocument().setFile(tempFile);

				if (nieuwUploadDocument)
				{
					fileService.saveOrUpdateUploadDocument(overeenkomst.getDocument(), FileStoreLocation.COLON_OVEREENKOMST);
				}
				else
				{
					fileService.updateUploadDocument(overeenkomst.getDocument());
				}
			}
			catch (Exception e)
			{
				LOG.error("Error wirting file", e);
			}
		}

		if (overeenkomst.getId() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_MODEL_GEMAAKT, account);
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_MODEL_GEWIJZIGD, account);

			for (AbstractAfgeslotenOvereenkomst afgeslotenOvereenkomst : overeenkomst.getAfgeslotenOvereenkomsten())
			{
				afgeslotenOvereenkomst.setNieuwereOvereenkomst(true);
				afgeslotenOvereenkomst.setAkkoordDatum(null);
				hibernateService.saveOrUpdate(afgeslotenOvereenkomst);
				verstuurOvereenkomstMail(afgeslotenOvereenkomst);
			}
		}

		hibernateService.saveOrUpdate(overeenkomst);

		if (OvereenkomstType.ZAKELIJKE_OVEREENKOMST == overeenkomst.getOvereenkomst())
		{
			cervixHuisartsSyncService.sendData(overeenkomst);
		}
	}

	@Override
	public List<Overeenkomst> getOvereenkomsten(Boolean actief, long first, long size, String sortProperty, boolean asc)
	{
		return overeenkomstDao.getOvereenkomsten(actief, first, size, sortProperty, asc);
	}

	@Override
	public long countOvereenkomsten(Boolean actief)
	{
		return overeenkomstDao.countOvereenkomsten(actief);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateOvereenkomst(Overeenkomst overeenkomst, Account account)
	{
		if (overeenkomst.isActief())
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_MODEL_GEACTIVEERD, account);

			for (AbstractAfgeslotenOvereenkomst afgeslotenOvereenkomst : overeenkomst.getAfgeslotenOvereenkomsten())
			{
				afgeslotenOvereenkomst.setEindDatum(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(afgeslotenOvereenkomst);
			}
		}
		else
		{

			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_MODEL_GEINACTIVEERD, account);
		}

		hibernateService.saveOrUpdate(overeenkomst);
	}

	@Override
	public <T> List<AbstractAfgeslotenOvereenkomst> getAfgeslotenOvereenkomsten(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Boolean actief, Long first,
		Long size, String sortProperty, boolean asc)
	{
		return overeenkomstDao.getAfgeslotenOvereenkomsten(returnType, filter, actief, first, size, sortProperty, asc);
	}

	@Override
	public <T> long countAfgeslotenOvereenkomsten(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Boolean actief)
	{
		return overeenkomstDao.countAfgeslotenOvereenkomsten(returnType, filter, actief);
	}

	@Override
	public List<Overeenkomst> getOvereenkomsten(OrganisatieType organisatieType, OvereenkomstType... overeenkomstTypes)
	{
		return overeenkomstDao.getOvereenkomsten(organisatieType, overeenkomstTypes);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateOvereenkomst(AbstractAfgeslotenOvereenkomst overeenkomst, FileUpload fileUpload, Account account)
	{
		boolean genereerCode = false;
		boolean verstuurMail = false;
		if (overeenkomst.getId() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_TOEGEVOEGD, account);
			overeenkomst.setVolgnummer(overeenkomstDao.getVolgnummerOvereenkomst());
			genereerCode = true;
			verstuurMail = overeenkomst.isTeAccoderen();
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_GEWIJZIGD, account);
			hibernateService.getHibernateSession().evict(overeenkomst);
			AbstractAfgeslotenOvereenkomst oudeAbstractAfgeslotenOvereenkomst = (AbstractAfgeslotenOvereenkomst) hibernateService.load(Hibernate.getClass(overeenkomst),
				overeenkomst.getId());
			if (!oudeAbstractAfgeslotenOvereenkomst.getOvereenkomst().equals(overeenkomst.getOvereenkomst())
				|| !oudeAbstractAfgeslotenOvereenkomst.getStartDatum().equals(overeenkomst.getStartDatum()))
			{
				overeenkomst.setAkkoordDatum(null);
				genereerCode = true;
			}

			if (overeenkomst.isTeAccoderen() && !oudeAbstractAfgeslotenOvereenkomst.isTeAccoderen())
			{
				verstuurMail = true;
			}

			hibernateService.getHibernateSession().evict(oudeAbstractAfgeslotenOvereenkomst);
		}

		if (genereerCode)
		{
			StringBuilder code = new StringBuilder();
			code.append(overeenkomst.getOvereenkomst().getNaam());
			code.append(".");
			if (overeenkomst.getScreeningOrganisatie().getRegioCode() != null)
			{
				code.append(overeenkomst.getScreeningOrganisatie().getRegioCode());
				code.append(".");
			}

			if (overeenkomst instanceof AfgeslotenMedewerkerOvereenkomst)
			{
				AfgeslotenMedewerkerOvereenkomst afgeslotenKwaliteitsOvereenkomst = (AfgeslotenMedewerkerOvereenkomst) overeenkomst;
				code.append(afgeslotenKwaliteitsOvereenkomst.getGebruiker().getId());
			}
			else if (overeenkomst instanceof AfgeslotenInstellingOvereenkomst)
			{
				AfgeslotenInstellingOvereenkomst afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) overeenkomst;
				code.append(afgeslotenOvereenkomst.getInstelling().getId());
			}

			code.append(".");
			code.append(new SimpleDateFormat("yyMM").format(overeenkomst.getStartDatum()));
			code.append(".");
			code.append(overeenkomst.getVolgnummer());
			overeenkomst.setCode(code.toString());
		}

		if (fileUpload != null)
		{
			File tempFile = null;
			try
			{
				tempFile = fileUpload.writeToTempFile();
				overeenkomst.setGescandDocument(new UploadDocument());
				UploadDocument gescandDocument = overeenkomst.getGescandDocument();
				gescandDocument.setContentType(fileUpload.getContentType());
				gescandDocument.setNaam(fileUpload.getClientFileName());
				gescandDocument.setFile(tempFile);
				gescandDocument.setActief(Boolean.TRUE);

				fileService.saveOrUpdateUploadDocument(gescandDocument, FileStoreLocation.COLON_OVEREENKOMST);
			}
			catch (Exception e)
			{
				LOG.error("Error wirting file", e);
			}
		}

		hibernateService.saveOrUpdate(overeenkomst);

		if (verstuurMail)
		{
			verstuurOvereenkomstMail(overeenkomst);
		}
	}

	protected void verstuurOvereenkomstMail(AbstractAfgeslotenOvereenkomst overeenkomst)
	{
		Gebruiker gebruiker = null;
		String emailUA = null;
		if (overeenkomst instanceof AfgeslotenInstellingOvereenkomst)
		{
			AfgeslotenInstellingOvereenkomst afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) overeenkomst;
			Instelling instelling = afgeslotenOvereenkomst.getInstelling();
			emailUA = instelling.getEmail();
			gebruiker = instelling.getGemachtigde();
		}
		else if (overeenkomst instanceof AfgeslotenMedewerkerOvereenkomst)
		{
			AfgeslotenMedewerkerOvereenkomst afgeslotenKwaliteitsOvereenkomst = (AfgeslotenMedewerkerOvereenkomst) overeenkomst;
			gebruiker = afgeslotenKwaliteitsOvereenkomst.getGebruiker();
		}

		if (gebruiker != null)
		{

			String content = null;
			boolean isUitstrijkendArts = overeenkomst.getOvereenkomst().getOrganisatieType() == OrganisatieType.HUISARTS;
			if (isUitstrijkendArts)
			{
				content = preferenceService.getString(PreferenceKey.OVEREEENKOMSTMAIL_ZVUA.name(), "{link}");
			}
			else
			{
				content = preferenceService.getString(PreferenceKey.OVEREEENKOMSTMAIL.name(), "{link}");
			}
			String link = applicationUrl;
			String aanhef = "";
			if (gebruiker.getAanhef() != null)
			{
				aanhef = " " + gebruiker.getAanhef().getNaam();
			}

			String titel = "";
			if (gebruiker.getTitel() != null)
			{
				titel = " " + gebruiker.getTitel().getNaam();
			}

			String achternaam = "";
			if (StringUtils.isNotBlank(gebruiker.getAchternaam()))
			{
				achternaam = " " + gebruiker.getAchternaam();
			}

			String tussenvoegsel = "";
			if (StringUtils.isNotBlank(gebruiker.getTussenvoegsel()))
			{
				tussenvoegsel = " " + gebruiker.getTussenvoegsel();
			}

			String voorletters = "";
			if (StringUtils.isNotBlank(gebruiker.getVoorletters()))
			{
				voorletters = " " + gebruiker.getVoorletters();
			}

			content = content.replaceAll("\\{link\\}", link);
			content = content.replaceAll("\\{aanhef\\}", aanhef);
			content = content.replaceAll("\\{titel\\}", titel);
			content = content.replaceAll("\\{achternaam\\}", achternaam);
			content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
			content = content.replaceAll("\\{voorletters\\}", voorletters);

			String emailAdres = gebruiker.getEmailwerk();
			if (StringUtils.isNotBlank(gebruiker.getEmailextra()))
			{
				emailAdres = gebruiker.getEmailextra();
			}
			else if (isUitstrijkendArts)
			{
				emailAdres = emailUA;
			}

			if (StringUtils.isNotBlank(emailAdres))
			{
				String onderwerp = null;
				if (isUitstrijkendArts)
				{
					onderwerp = preferenceService.getString(PreferenceKey.OVEREENKOMSTSUBJECT_ZVUA.name(), "Zakelijke voorwaarden gewijzigd");
				}
				else
				{
					onderwerp = preferenceService.getString(PreferenceKey.OVEREENKOMSTSUBJECT.name(), "Overeenkomst gewijzigd");
				}
				mailService.sendEmail(emailAdres, onderwerp, content);
			}
		}
	}

	@Override
	public List<AbstractAfgeslotenOvereenkomst> getTeAccoderenOvereenkomsten(InstellingGebruiker inTeLoggenInstellingGebruiker)
	{
		return overeenkomstDao.getTeAccoderenOvereenkomsten(inTeLoggenInstellingGebruiker);
	}

	@Override
	public long countTeAccoderenOvereenkomsten(InstellingGebruiker inTeLoggenInstellingGebruiker)
	{
		return overeenkomstDao.countTeAccoderenOvereenkomsten(inTeLoggenInstellingGebruiker);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void accodeerOvereenkomsten(InstellingGebruiker instellingGebruiker, Account account)
	{
		for (AbstractAfgeslotenOvereenkomst afgeslotenOvereenkomst : getTeAccoderenOvereenkomsten(instellingGebruiker))
		{
			afgeslotenOvereenkomst.setAkkoordDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(afgeslotenOvereenkomst);
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_GEACCODEERD, account);
		}
	}

	@Override
	public List<Instelling> getAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter, String sortProperty, boolean ascending, int first, int count)
	{
		return overeenkomstDao.getAfgeslotenOvereenkomsten(filter, sortProperty, ascending, first, count);
	}

	@Override
	public long countAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter)
	{
		return overeenkomstDao.countAfgeslotenOvereenkomsten(filter);
	}

	@Override
	public List<AfgeslotenInstellingOvereenkomst> getAfgeslotenOvereenkomstenBijInstelling(OvereenkomstZoekFilter filter, Instelling instelling)
	{
		return overeenkomstDao.getAfgeslotenOvereenkomstenBijInstelling(filter, instelling);
	}

	@Override
	public List<Overeenkomst> getAlleOvereenkomstenVanTypeOvereenkomst()
	{
		return overeenkomstDao.getAlleOvereenkomstenVanTypeOvereenkomst();
	}
}

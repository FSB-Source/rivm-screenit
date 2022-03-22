package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.dao.VragenlijstDao;
import nl.rivm.screenit.main.service.FormulierService;
import nl.rivm.screenit.main.service.VragenlijstService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectVragenlijstUitzettenVia;
import nl.rivm.screenit.model.vragenlijsten.Vragenlijst;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoorden;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoordenHolder;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class VragenlijstServiceImpl implements VragenlijstService
{
	@Autowired
	private VragenlijstDao vragenlijstDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private FormulierService formulierService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateVragenlijst(Vragenlijst vragenlijst, File definitieFile, InstellingGebruiker uploader) throws Exception
	{
		TypeFormulier typeFormulier = TypeFormulier.VRAGENLIJST;

		try
		{
			vragenlijst.setLaatstGewijzigd(new Date());
			if (vragenlijst.getActief() == null)
			{
				vragenlijst.setActief(Boolean.TRUE);
			}

			if (definitieFile != null)
			{
				hibernateService.saveOrUpdate(vragenlijst); 
				List<ScreenitFormulierInstantie> definities = formulierService.importFormulier(typeFormulier, definitieFile,
					typeFormulier.name() + "_" + vragenlijst.getId().toString());

				if (!definities.isEmpty())
				{
					ScreenitFormulierInstantie oldFormulierInstantie = vragenlijst.getFormulierInstantie();
					ScreenitFormulierInstantie nieuweFormulierInstantie = definities.get(0);
					vragenlijst.setFormulierInstantie(nieuweFormulierInstantie);
					nieuweFormulierInstantie.setUploader(uploader);
					if (oldFormulierInstantie != null)
					{
						vervangOudeFormulierDefinities(typeFormulier, oldFormulierInstantie, nieuweFormulierInstantie);
					}
					hibernateService.saveOrUpdate(nieuweFormulierInstantie);
				}
				hibernateService.saveOrUpdate(vragenlijst);
			}
		}
		catch (InvalidFormatException | ClassNotFoundException | IOException | IllegalStateException e)
		{
			throw e;
		}

		hibernateService.saveOrUpdate(vragenlijst);
	}

	private void vervangOudeFormulierDefinities(TypeFormulier typeFormulier, ScreenitFormulierInstantie oldFormulierInstantie, ScreenitFormulierInstantie nieuweFormulierInstantie)
	{
		boolean oudeFormulierDefinitieMagVerwijderderdWorden = true;

		List<ProjectBrief> brieven = vragenlijstDao.getAllProjectBrievenWithFormulier(oldFormulierInstantie);

		for (ProjectBrief brief : brieven)
		{
			if (ProjectVragenlijstUitzettenVia.WEB.equals(brief.getDefinitie().getProjectVragenlijstUitzettenVia()))
			{
				VragenlijstAntwoorden<?> vragenlijstAntwoorden = brief.getVragenlijstAntwoordenHolder().getVragenlijstAntwoorden();
				if (vragenlijstAntwoorden.getResultaat() != null)
				{
					oudeFormulierDefinitieMagVerwijderderdWorden = false;
					break;
				}
				else
				{
					vragenlijstAntwoorden.setFormulierInstantie(nieuweFormulierInstantie);
					hibernateService.saveOrUpdate(vragenlijstAntwoorden);
				}
			}
			else
			{
				oudeFormulierDefinitieMagVerwijderderdWorden = false;
				break;
			}
		}

		if (oudeFormulierDefinitieMagVerwijderderdWorden)
		{
			if (oldFormulierInstantie.getTemplateVanGebruiker() != null)
			{
				uploadDocumentService.delete(oldFormulierInstantie.getTemplateVanGebruiker(), true);
			}
			hibernateService.delete(oldFormulierInstantie);
		}
	}

	@Override
	public Iterator<ScreenitFormulierInstantie> getAlleFormulierenMetResultatenEnHuidige(Vragenlijst vragenlijst)
	{
		return vragenlijstDao.getAlleFormulierenMetResultatenEnHuidige(vragenlijst);
	}

	@Override
	public long countAlleFormulierenMetResultatenEnHuidige(Vragenlijst vragenlijst)
	{
		return vragenlijstDao.countAlleFormulierenMetResultatenEnHuidige(vragenlijst);
	}

	@Override
	public boolean isVragenlijstGekoppeldAanHolder(Long vragenlijstId, Class<? extends VragenlijstAntwoordenHolder> holder)
	{
		return vragenlijstDao.isVragenlijstGekoppeldAanHolder(vragenlijstId, holder);
	}

	@Override
	public void saveOrUpdateTemplate(ScreenitFormulierInstantie formulierInstantie, String contentType, String clientFileName, File tempFile) throws IOException
	{
		UploadDocument document = new UploadDocument();
		document.setActief(true);
		document.setContentType(contentType);
		document.setNaam(clientFileName);
		document.setFile(tempFile);
		uploadDocumentService.saveOrUpdate(document, FileStoreLocation.VRAGENLIJSTEN_TEMPLATES);

		UploadDocument templateVanGebruiker = formulierInstantie.getTemplateVanGebruiker();
		formulierInstantie.setTemplateVanGebruiker(document);
		hibernateService.saveOrUpdate(formulierInstantie);
		if (templateVanGebruiker != null)
		{
			uploadDocumentService.delete(templateVanGebruiker, true);
		}
	}
}

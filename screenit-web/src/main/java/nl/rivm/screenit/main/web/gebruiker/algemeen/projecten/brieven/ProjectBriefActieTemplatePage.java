package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven;

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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.util.EnumStringUtil;
import nl.rivm.screenit.main.web.component.table.NavigeerNaarCellPanel;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadLinkPanel;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectVragenlijstUitzettenVia;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

public class ProjectBriefActieTemplatePage extends ProjectTemplateTestenBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ProjectBriefActieTemplatePage.class);

	private static final String DATUM_PATROON = "dd-MM-yyyy";

	private IModel<ProjectBriefActie> briefactieModel;

	@SpringBean
	private LogService logService;

	@SpringBean
	private FileService fileService;

	@SpringBean
	private AsposeService asposeService;

	@SpringBean
	private HibernateService hibernateService;

	private IModel<ScreeningOrganisatie> selectedInstelling;

	public ProjectBriefActieTemplatePage(IModel<Project> model, IModel<ProjectBriefActie> briefactieModel)
	{
		super(model);
		this.briefactieModel = briefactieModel;

		maakTerugKnop();
	}

	@Override
	protected List<ScreeningOrganisatie> getRegios()
	{
		List<ScreeningOrganisatie> soLijst = new ArrayList<ScreeningOrganisatie>();
		if (briefactieModel.getObject().getProject() != null && briefactieModel.getObject().getProject().getScreeningOrganisaties() != null)
		{
			for (Instelling instelling : briefactieModel.getObject().getProject().getScreeningOrganisaties())
			{
				ScreeningOrganisatie organisatie = hibernateService.load(ScreeningOrganisatie.class, instelling.getId());
				soLijst.add(organisatie);
			}
		}
		return soLijst;
	}

	private void maakTerugKnop()
	{
		add(new Link<Void>("terugViaTitle")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(new ProjectBriefActiePage(ProjectBriefActieTemplatePage.this.getModel()));
			}

			@Override
			protected void onDetach()
			{
				super.onDetach();
				ModelUtil.nullSafeDetach(briefactieModel);
			}
		});
	}

	private String getHerrineringTekst()
	{
		String tekst = "";
		ProjectBriefActie projectBriefActie = briefactieModel.getObject();
		ProjectBriefActie herinnerActie = projectBriefActie.getHerinneringsActie();
		if (herinnerActie != null)
		{
			ProjectBriefActieType type = projectBriefActie.getType();
			tekst = herinnerActie.getAantalDagen() + " dagen na een " + getString("ProjectBriefActieType." + type.name());
		}
		return tekst;
	}

	private String getLaatstGewijzigdDatum()
	{
		ProjectBriefActie projectBriefActie = briefactieModel.getObject();
		String laatstGewijzigd = "";
		Date datum = projectBriefActie.getLaatstGewijzigd();
		if (datum != null)
		{
			SimpleDateFormat format = new SimpleDateFormat(DATUM_PATROON);
			laatstGewijzigd = format.format(datum);
		}
		return laatstGewijzigd;
	}

	private String getSoortText()
	{
		ProjectBriefActie projectBriefActie = briefactieModel.getObject();
		String soortText = "";
		if (projectBriefActie != null && projectBriefActie.getType() != null)
		{
			soortText = getString(EnumStringUtil.getPropertyString(projectBriefActie.getType()));
		}
		return soortText;
	}

	private String getMomentText()
	{
		ProjectBriefActie projectBriefActie = briefactieModel.getObject();
		String momentTekst = "";
		ProjectBriefActieType type = projectBriefActie.getType();

		if (ProjectBriefActieType.DATUM.equals(type))
		{
			SimpleDateFormat format = new SimpleDateFormat(DATUM_PATROON);
			momentTekst = format.format(projectBriefActie.getDatum());
		}
		else if (ProjectBriefActieType.XDAGENNAY.equals(type))
		{
			momentTekst = projectBriefActie.getAantalDagen() + " dagen na ";
			BriefType briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += getString(EnumStringUtil.getPropertyString(briefType));
			}
		}
		else if (ProjectBriefActieType.XMETY.equals(type))
		{
			momentTekst = "Tegelijk met ";
			BriefType briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += getString(EnumStringUtil.getPropertyString(briefType));
			}
		}
		else if (ProjectBriefActieType.VERVANGENDEBRIEF.equals(type))
		{
			BriefType briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += getString(EnumStringUtil.getPropertyString(briefType));
			}
		}
		return momentTekst;
	}

	@SuppressWarnings("unchecked")
	private IModel<Project> getModel()
	{
		return (IModel<Project>) getDefaultModel();
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(briefactieModel);
		ModelUtil.nullSafeDetach(selectedInstelling);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return ProjectBriefActiePage.class;
	}

	@Override
	protected void addAdditionalFormComponents(Form<Void> form)
	{
		form.add(new Label("naam", new PropertyModel<>(briefactieModel, "document.naam")));
		form.add(new Label("soort", Model.of(getSoortText())));
		form.add(new Label("moment", Model.of(getMomentText())));
		form.add(new Label("laatstGewijzigd", Model.of(getLaatstGewijzigdDatum())));
		form.add(new Label("vragenlijst", new PropertyModel<>(briefactieModel, "vragenlijst.naam")));
		form.add(new Label("herrinering", Model.of(getHerrineringTekst())));
		form.add(new Label("uitzettenVia", new PropertyModel<>(briefactieModel, "projectVragenlijstUitzettenVia")));
		form.add(new Label("orionWerkbak", new PropertyModel<>(briefactieModel, "misluktBak")));
		form.add(new Label("formulierNummer", new PropertyModel<>(briefactieModel, "formulierNummer")));
		form.add(new NavigeerNaarCellPanel<ProjectBriefActie>("herrinneringPrinten", briefactieModel)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected boolean magNavigerenNaar(IModel<ProjectBriefActie> rowModel)
			{
				return rowModel.getObject() != null && rowModel.getObject().getHerinneringsActie() != null;
			}

			@Override
			protected void onNavigeerNaar(AjaxRequestTarget target, IModel<ProjectBriefActie> rowModel)
			{
				setResponsePage(new ProjectBriefHerinneringTemplatePage(ModelUtil.sModel((Project) ProjectBriefActieTemplatePage.this.getDefaultModelObject()),
					ModelUtil.sModel(rowModel.getObject())));
			}
		});
		form.add(new UploadDocumentDownloadLinkPanel("herrinneringDownloaden", new PropertyModel<UploadDocument>(briefactieModel, "herinneringsActie.document")));

	}

	@Override
	protected List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return briefactieModel.getObject().getProject().getBevolkingsonderzoeken();
	}

	@Override
	protected Document proccesDocument(MailMergeContext context, File briefTemplate) throws Exception
	{
		ProjectBriefActie projectBriefActie = briefactieModel.getObject();
		ScreenitFormulierInstantie vragenlijstInstantie = null;
		if (projectBriefActie.getVragenlijst() != null && ProjectVragenlijstUitzettenVia.isPapier(projectBriefActie.getProjectVragenlijstUitzettenVia()))
		{
			vragenlijstInstantie = projectBriefActie.getVragenlijst().getFormulierInstantie();
		}
		Document mergedDocument = asposeService.processDocument(briefTemplate, context);
		Document vragenlijst = null;
		if (vragenlijstInstantie != null)
		{
			if (vragenlijstInstantie.getTemplateVanGebruiker() == null)
			{
				vragenlijst = asposeService.processVragenlijst(context, vragenlijstInstantie, true);
			}
			else
			{
				File vragenlijstTemplate = fileService.load(vragenlijstInstantie.getTemplateVanGebruiker());
				vragenlijst = asposeService.processDocument(vragenlijstTemplate, context);
			}
			if (vragenlijst != null)
			{
				mergedDocument.getLastSection().getHeadersFooters().linkToPrevious(false);
				mergedDocument.appendDocument(vragenlijst, ImportFormatMode.KEEP_SOURCE_FORMATTING);
			}
		}
		return mergedDocument;
	}

	@Override
	protected File getBriefTemplateFile()
	{
		return fileService.load(briefactieModel.getObject().getDocument());
	}
}

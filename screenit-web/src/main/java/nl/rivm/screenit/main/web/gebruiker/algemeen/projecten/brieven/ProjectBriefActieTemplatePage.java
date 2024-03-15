package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.component.table.NavigeerNaarCellPanel;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadLinkPanel;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectVragenlijstUitzettenVia;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnumStringUtil;
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

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

@Slf4j
public class ProjectBriefActieTemplatePage extends ProjectTemplateTestenBasePage
{
	private IModel<ProjectBriefActie> briefactieModel;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private AsposeService asposeService;

	@SpringBean
	private HibernateService hibernateService;

	public ProjectBriefActieTemplatePage(IModel<Project> model, IModel<ProjectBriefActie> briefactieModel)
	{
		super(model);
		this.briefactieModel = briefactieModel;

		maakTerugKnop();
	}

	@Override
	protected List<ScreeningOrganisatie> getRegios()
	{
		List<ScreeningOrganisatie> soLijst = new ArrayList<>();
		Project project = briefactieModel.getObject().getProject();
		if (project != null && project.getScreeningOrganisaties() != null)
		{
			for (Instelling instelling : project.getScreeningOrganisaties())
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
			@Override
			public void onClick()
			{
				setResponsePage(new ProjectBriefActiePage(getProjectModel()));
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
			laatstGewijzigd = DateUtil.formatShortDate(datum);
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
			momentTekst = DateUtil.formatShortDate(projectBriefActie.getDatum());
		}
		else if (ProjectBriefActieType.XDAGENNAY.equals(type))
		{
			momentTekst = projectBriefActie.getAantalDagen() + " dagen na ";
			BriefType briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += briefType.getWeergaveNaam();
			}
		}
		else if (ProjectBriefActieType.XMETY.equals(type))
		{
			momentTekst = "Tegelijk met ";
			BriefType briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += briefType.getWeergaveNaam();
			}
		}
		else if (ProjectBriefActieType.VERVANGENDEBRIEF.equals(type))
		{
			BriefType briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += briefType.getWeergaveNaam();
			}
		}
		return momentTekst;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(briefactieModel);
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
		form.add(new NavigeerNaarCellPanel<>("herrinneringPrinten", briefactieModel)
		{
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
		form.add(new UploadDocumentDownloadLinkPanel("herrinneringDownloaden", new PropertyModel<>(briefactieModel, "herinneringsActie.document")));

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
		Document vragenlijst;
		if (vragenlijstInstantie != null)
		{
			if (vragenlijstInstantie.getTemplateVanGebruiker() == null)
			{
				vragenlijst = asposeService.processVragenlijst(context, vragenlijstInstantie, true);
			}
			else
			{
				File vragenlijstTemplate = uploadDocumentService.load(vragenlijstInstantie.getTemplateVanGebruiker());
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
		return uploadDocumentService.load(briefactieModel.getObject().getDocument());
	}

}

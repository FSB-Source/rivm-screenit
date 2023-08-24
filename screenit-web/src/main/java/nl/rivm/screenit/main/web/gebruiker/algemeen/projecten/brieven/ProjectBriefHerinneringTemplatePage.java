package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.aspose.words.Document;

public class ProjectBriefHerinneringTemplatePage extends ProjectTemplateTestenBasePage
{
	@SpringBean
	private UploadDocumentService uploadDocumentService;

	private final IModel<ProjectBriefActie> briefactieModel;

	public ProjectBriefHerinneringTemplatePage(IModel<Project> model, IModel<ProjectBriefActie> briefactieModel)
	{
		super(model);
		this.briefactieModel = briefactieModel;

		maakTerugKnop();

	}

	private ProjectBriefActie getHerinneringsActie()
	{
		return briefactieModel.getObject().getHerinneringsActie();
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
		add(new Link<Void>("terugViaTitleTwee")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ProjectBriefActieTemplatePage(getProjectModel(), briefactieModel));
			}
		});
	}

	private String getMomentText()
	{
		String tekst = "";
		ProjectBriefActie projectBriefActie = briefactieModel.getObject();
		ProjectBriefActie herinnerActie = getHerinneringsActie();
		if (herinnerActie != null)
		{
			ProjectBriefActieType type = projectBriefActie.getType();
			tekst = herinnerActie.getAantalDagen() + " dagen na een " + getString("ProjectBriefActieType." + type.name());
		}
		return tekst;
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
	protected List<ScreeningOrganisatie> getRegios()
	{
		List<ScreeningOrganisatie> soLijst = new ArrayList<>();
		if (getHerinneringsActie().getProject() != null && getHerinneringsActie().getProject().getScreeningOrganisaties() != null)
		{
			for (Instelling instelling : getHerinneringsActie().getProject().getScreeningOrganisaties())
			{
				soLijst.add((ScreeningOrganisatie) instelling);
			}
		}
		return soLijst;
	}

	@Override
	protected void addAdditionalFormComponents(Form<Void> form)
	{
		form.add(new Label("naam", new PropertyModel<>(briefactieModel, "herinneringsActie.document.naam")));
		form.add(new Label("moment", Model.of(getMomentText())));
	}

	@Override
	protected List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return briefactieModel.getObject().getProject().getBevolkingsonderzoeken();
	}

	@Override
	protected Document proccesDocument(MailMergeContext context, File briefTemplate) throws Exception
	{
		return asposeService.processDocument(briefTemplate, context);
	}

	@Override
	protected File getBriefTemplateFile()
	{
		return uploadDocumentService.load(getHerinneringsActie().getDocument());
	}
}

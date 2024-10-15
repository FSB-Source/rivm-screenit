package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenWriter;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.ProjectBrievenConstants;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectMergedBrieven;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.ClientService;

import org.apache.commons.lang.StringUtils;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.aspose.words.Document;

@Component
public class ProjectBrievenGenererenWriter extends AbstractBrievenGenererenWriter<ProjectBrief, ProjectMergedBrieven>
{

	@Autowired
	private BaseProjectService projectService;

	@Autowired
	private ClientService clientService;

	@Override
	protected ProjectMergedBrieven createConcreteMergedBrieven(Date aangemaaktOp)
	{
		ExecutionContext context = getStepExecutionContext();
		ScreeningOrganisatie screeningOrganisatie = getHibernateService().load(ScreeningOrganisatie.class,
			context.getLong(ProjectBrievenConstants.KEY_PROJECT_SCREENINGORGANISATIE_ID));

		ProjectMergedBrieven mergedBrieven = new ProjectMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(screeningOrganisatie);
		mergedBrieven.setCreatieDatum(aangemaaktOp);
		getHibernateService().saveOrUpdate(mergedBrieven);
		return mergedBrieven;
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return ProjectBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	public void additionalActiesWithDocument(MailMergeContext context, ProjectBrief projectBrief, Document chunkDocument) throws Exception
	{

		ProjectBriefActie actie = getHibernateService().load(ProjectBriefActie.class, getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID));

		projectService.addVragenlijstAanTemplate(context, chunkDocument, actie, projectBrief);
	}

	@Override
	public void additionalMergedContext(MailMergeContext context)
	{
		ProjectBrief brief = (ProjectBrief) context.getBrief();
		ClientBrief<?, ?, ?> orgineleBrief = brief.getBrief();
		if (orgineleBrief != null)
		{
			switch (orgineleBrief.getBevolkingsonderzoek())
			{
			case COLON:
				ColonBrief colonBrief = (ColonBrief) getHibernateService().deproxy(orgineleBrief);
				context.setIntakeAfspraak(colonBrief.getIntakeAfspraak());
				context.setVorigeIntakeAfspraak(colonBrief.getVorigeIntakeAfspraak());
				break;
			case CERVIX:
				CervixBrief cervixBrief = (CervixBrief) getHibernateService().deproxy(orgineleBrief);
				if (cervixBrief.getUitnodiging() != null)
				{
					context.setCervixUitnodiging(cervixBrief.getUitnodiging());
				}
				else if (cervixBrief.getMonster() != null)
				{
					context.setCervixUitnodiging(cervixBrief.getMonster().getUitnodiging());
				}
				else if (cervixBrief.getLabformulier() != null)
				{
					context.setCervixUitnodiging(cervixBrief.getLabformulier().getUitstrijkje().getUitnodiging());
				}
				break;
			}
		}
		if (brief.getProjectClient().getProject().getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.MAMMA))
		{
			context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, clientService.bepaalCe(context.getClient()));
		}
		context.setProjectBrief(brief);
		if (brief != null && brief.getDefinitie().getVragenlijst() != null)
		{
			context.setVragenlijstNaam(brief.getDefinitie().getVragenlijst().getNaam());
		}

		ProjectClient projectClient = brief.getProjectClient();
		Project project = projectClient.getProject();
		List<ProjectClientAttribuut> projectClientAttributen = projectClient.getAttributen();
		for (ProjectAttribuut attribuut : project.getProjectAttributen())
		{
			if (!attribuut.getActief())
			{
				continue;
			}
			String value = null;
			for (ProjectClientAttribuut papc : projectClientAttributen)
			{
				if (attribuut.equals(papc.getAttribuut()))
				{
					value = papc.getValue();
				}
			}
			context.getProjectAttributen().put(attribuut, value);
		}
	}

	@Override
	public String getMergedBrievenNaam(ProjectMergedBrieven brieven)
	{
		ProjectBriefActie actie = getHibernateService().load(ProjectBriefActie.class, getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID));
		String naam = "";
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd_HH.mm");
		if (brieven.getCreatieDatum() != null)
		{
			naam += sdf.format(brieven.getCreatieDatum()) + "-";
		}
		if (brieven.getScreeningOrganisatie() != null)
		{
			String soNaam = brieven.getScreeningOrganisatie().getNaam();
			soNaam = soNaam.replaceAll(" ", "_");
			naam += soNaam + "-";
		}
		if (actie.getProject().getNaam() != null)
		{
			String projectNaam = actie.getProject().getNaam();
			projectNaam = projectNaam.replaceAll(" ", "_");
			naam += projectNaam + "-";
		}
		if (StringUtils.isNotEmpty(actie.getPrintomschrijving()))
		{
			naam += actie.getPrintomschrijving().replace(" ", "_").toLowerCase();
		}
		naam = addPdfCounter(naam);

		return naam + ".pdf";
	}

	@Override
	public IDocument getDocumentDefinitie()
	{
		Long projectBriefActieId = getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID);
		return getHibernateService().load(ProjectBriefActie.class, projectBriefActieId);
	}

	@Override
	public String getTechnischeLoggingMergedBriefAanmaken(ProjectMergedBrieven brieven)
	{
		Long projectBriefActieId = getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID);
		ProjectBriefActie actie = getHibernateService().load(ProjectBriefActie.class, projectBriefActieId);

		String tekst = "Mergedocument(id = " + brieven.getId() + ") aangemaakt voor ScreeningOrganisatie " + brieven.getScreeningOrganisatie().getNaam();
		if (actie != null)
		{
			if (actie.getProject() != null)
			{
				tekst = tekst + ", project " + actie.getProject().getNaam() + ", projectactie(" + actie.getId() + ") " + actie.getType().name();
			}
		}
		return tekst;
	}

	@Override
	public FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.PROJECT_MERGED_BRIEVEN;
	}

	@Override
	public Long getFileStoreId()
	{
		ExecutionContext stepExecutionContext = getStepExecutionContext();

		ProjectBriefActie actie = getHibernateService().load(ProjectBriefActie.class, stepExecutionContext.getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID));
		Project project = actie.getProject();
		return project.getId();
	}

	@Override
	public LogGebeurtenis getMergeProbleemLogGebeurtenis()
	{
		return LogGebeurtenis.PROJECT_BRIEF_MERGE_FOUT;
	}

	@Override
	public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
	{
		return LogGebeurtenis.PROJECT_ONVOLLEDIG_ADRES;
	}
}

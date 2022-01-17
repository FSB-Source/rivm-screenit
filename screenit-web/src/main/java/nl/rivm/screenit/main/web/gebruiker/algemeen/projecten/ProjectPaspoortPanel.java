
package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten;

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

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ProjectPaspoortPanel extends GenericPanel<Project>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ProjectService projectService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public ProjectPaspoortPanel(String id, IModel<Project> model)
	{
		super(id, model);
		hibernateService.reload(model.getObject());

	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		IModel<Project> model = getModel();
		ProjectType projectType = model.getObject().getType();

		add(new Label("passpoortNaam", Model.of(getProjectNaamEnBvo(model))));
		add(new EnumLabel("passpoortStatus", ProjectUtil.getStatus(model.getObject(), currentDateSupplier.getDate())));
		add(new EnumLabel<ProjectType>("passpoortType", projectType));
		add(new EnumLabel("passpoortGroepSelectieType", new PropertyModel<String>(model, "groepSelectieType")));
		add(new Label("passpoortInactief", Model.of(getTotaalInactieveClientenProject(model.getObject()))));
		add(new Label("passpoortPopulatie", Model.of(getTotaalClientenProject(model.getObject()))));
		add(new Label("passpoortExcludeerAfmelding", Model.of(Bevolkingsonderzoek.getAfkortingen(model.getObject().getExcludeerAfmelding())))
			.setVisible(!ProjectType.BRIEFPROJECT.equals(projectType)));
		add(new Label("passpoortExcludeerOpenRonde", Model.of(Bevolkingsonderzoek.getAfkortingen(model.getObject().getExcludeerOpenRonde())))
			.setVisible(!ProjectType.BRIEFPROJECT.equals(projectType)));
		add(new Label("passpoortExcludeerBezwaarmakers", Model.of(getProjectExcludeerBezwaar(model.getObject())))
			.setVisible(!ProjectType.BRIEFPROJECT.equals(projectType)));
	}

	private Long getTotaalClientenProject(Project project)
	{
		return projectService.getAantalProjectClientenVanProject(project);
	}

	private int getTotaalInactieveClientenProject(Project project)
	{
		return projectService.getAantalInactieveProjectClientenVanProject(project).intValue();
	}

	private String getProjectNaamEnBvo(IModel<Project> model)
	{
		Project project = model.getObject();
		String projectNaam = project.getNaam();
		if (project.getBevolkingsonderzoeken() != null && !project.getBevolkingsonderzoeken().isEmpty())
		{
			projectNaam += " (" + Bevolkingsonderzoek.getAfkortingen(project.getBevolkingsonderzoeken()) + ")";
		}
		return projectNaam;
	}

	private String getProjectExcludeerBezwaar(Project project)
	{
		if (Boolean.TRUE.equals(project.getExcludeerBezwaar()))
		{
			return "Ja";
		}
		return "Nee";
	}
}

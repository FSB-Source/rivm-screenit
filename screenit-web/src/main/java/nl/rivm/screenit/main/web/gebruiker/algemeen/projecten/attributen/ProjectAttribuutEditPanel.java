package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.attributen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import ca.uhn.hl7v2.util.StringUtil;

public abstract class ProjectAttribuutEditPanel extends GenericPanel<ProjectAttribuut>
{

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private ProjectService projectService;

	private final String oudeNaamAttribuut;

	private final IModel<ProjectAttribuut> attributeModel;

	public ProjectAttribuutEditPanel(String id, IModel<ProjectAttribuut> model)
	{
		super(id, model);
		attributeModel = model;
		oudeNaamAttribuut = model.getObject().getNaam();

		Form<ProjectAttribuut> form = new Form<>("form", attributeModel);
		add(form);

		ComponentHelper.addTextField(form, "naam", true, 50, false);

		form.add(ComponentHelper.newCheckBox("nietZichtbaarInClientDossier").setEnabled(attributeModel.getObject().getId() == null));
		form.add(ComponentHelper.newCheckBox("barcode"));

		add(new IndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ProjectAttribuut attribuut = form.getModelObject();
				LogGebeurtenis logGebeurtenis = attribuut.getId() == null ? LogGebeurtenis.PROJECT_ATTRIBUUT_TOEGEVOEGD : LogGebeurtenis.PROJECT_ATTRIBUUT_GEWIJZIGD;

				String naamAttribuut = attribuut.getNaam();
				naamAttribuut = naamAttribuut.replaceAll(" ", "").toLowerCase();

				String projectNaam = attribuut.getProject().getNaam();
				projectNaam = projectNaam.replaceAll(" ", "").toLowerCase();

				String mergeField = "_" + projectNaam + "_" + naamAttribuut;

				attribuut.setMergeField(mergeField);
				if (projectService.getProjectAttribuut(attribuut) == null)
				{
					hibernateService.saveOrUpdate(attribuut);

					projectService.projectAttribuutOpslaan(attribuut);

					String melding = "Project: " + attribuut.getProject().getNaam() + " attribuutnaam: " + attribuut.getNaam();
					if (StringUtil.isNotBlank(oudeNaamAttribuut) && LogGebeurtenis.PROJECT_ATTRIBUUT_GEWIJZIGD == logGebeurtenis)
					{
						melding += " oude attribuutnaam: " + oudeNaamAttribuut;
					}
					logService.logGebeurtenis(logGebeurtenis, ScreenitSession.get().getLoggedInAccount(), melding);

					opslaan(target);
				}
				else
				{
					error("Er is al een attribuut in dit project met zelfde naam of zelfde mergefield.");
				}
			}

		});
	}

	protected abstract void opslaan(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(attributeModel);
	}

}

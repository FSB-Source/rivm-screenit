package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingenPerSeDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.beans.support.PropertyComparator;

public abstract class MammaRouteConceptAnnulerenMeldingenDialogPanel extends GenericPanel<PlanningConceptMeldingenDto>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@SpringBean
	private HibernateService hibernateService;

	public MammaRouteConceptAnnulerenMeldingenDialogPanel(String id, Model<PlanningConceptMeldingenDto> model)
	{
		super(id, model);

		boolean heeftMeldingen = !model.getObject().seMeldingen.isEmpty();
		WebMarkupContainer title = new WebMarkupContainer("title");
		WebMarkupContainer titleGeenWijzigingen = new WebMarkupContainer("title.geen.wijzigingen");
		add(title.setVisible(heeftMeldingen));
		add(titleGeenWijzigingen.setVisible(!heeftMeldingen));
		WebMarkupContainer listContainer = new WebMarkupContainer("listContainer");
		add(listContainer.setVisible(heeftMeldingen));
		listContainer.add(createMeldingenView(model));
		listContainer.setOutputMarkupId(true);
		add(new IndicatingAjaxLink<Void>("doorvoeren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				baseConceptPlanningsApplicatie.conceptAnnuleren(ScreenitSession.get().getLoggedInInstellingGebruiker());
				ScreenitSession.get().success(getString("concept.geannuleerd"));
				close(target);
			}
		}.setVisible(model.getObject().niveau != MammaMeldingNiveau.PROBLEEM && heeftMeldingen));
	}

	private Component createMeldingenView(IModel<PlanningConceptMeldingenDto> model)
	{
		final WebMarkupContainer container = new WebMarkupContainer("container");
		List<MammaScreeningsEenheid> screeningsEenheden = model.getObject().seMeldingen.keySet().stream().map(seId -> hibernateService.load(MammaScreeningsEenheid.class, seId))
			.collect(Collectors.toList());
		Collections.sort(screeningsEenheden, new PropertyComparator<>("code", false, true));
		container.add(new ListView<MammaScreeningsEenheid>("seMeldingen", ModelUtil.listRModel(screeningsEenheden, false))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<MammaScreeningsEenheid> item)
			{
				MammaScreeningsEenheid screeningsEenheid = item.getModelObject();
				PlanningMeldingenPerSeDto meldingenPerSeDto = model.getObject().seMeldingen.get(screeningsEenheid.getId());
				Component content = createMeldingenView(meldingenPerSeDto.meldingen);
				WebMarkupContainer collapseLink = new WebMarkupContainer("collapseLink");
				collapseLink.add(new AttributeAppender("data-parent", Model.of("#" + container.getMarkupId())));
				collapseLink.add(new AttributeAppender("href", Model.of("#" + content.getMarkupId())));
				item.add(collapseLink);

				collapseLink.add(new Label("collapseName", screeningsEenheid.getCode() + " (#" + meldingenPerSeDto.meldingen.size() + ")"));
				item.add(content);
			}
		});
		return container;
	}

	private Component createMeldingenView(List<PlanningMeldingDto> meldingen)
	{
		WebMarkupContainer content = new WebMarkupContainer("content");
		content.add(new ListView<PlanningMeldingDto>("meldingen", meldingen)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<PlanningMeldingDto> item)
			{
				item.setVisible(item.getModelObject().niveau == MammaMeldingNiveau.INFO);
				item.setDefaultModel(new CompoundPropertyModel<>(item.getModel()));
				item.add(new Label("tekst"));
			}
		});
		return content;
	}

	protected abstract void close(AjaxRequestTarget target);

}

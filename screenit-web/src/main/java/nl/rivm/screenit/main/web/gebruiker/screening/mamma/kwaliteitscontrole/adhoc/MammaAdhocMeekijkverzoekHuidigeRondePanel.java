package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.adhoc;

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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractBEAccordionPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaAdhocMeekijkverzoekHuidigeRondePanel extends AbstractBEAccordionPanel<MammaOnderzoek>
{
	private static final String ID_GEZIEN = "next";

	private final IModel<MammaAdhocMeekijkverzoek> adhocMeekijkverzoekOnderzoekModel;

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	private List<Component> buttons = new ArrayList<>();

	public MammaAdhocMeekijkverzoekHuidigeRondePanel(String id, IModel<MammaOnderzoek> model, IModel<MammaAdhocMeekijkverzoek> adhocMeekijkverzoekOnderzoekModel)
	{
		super(id, model, 12);
		this.adhocMeekijkverzoekOnderzoekModel = adhocMeekijkverzoekOnderzoekModel;
		setIngeklapt(false);
		MammaOnderzoek onderzoek = getModelObject();
		LocalDate localDate = DateUtil.toLocalDate(onderzoek.getCreatieDatum());

		setTitle(Model.of("" + localDate.getYear()));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		renderPanelComponents();
	}

	protected void createButtons(WebMarkupContainer panelContainer, List<Component> buttons)
	{
		addButton(panelContainer, buttons, ID_GEZIEN, MammaVisitatieOnderzoekStatus.GEZIEN,
			ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_IMS_KOPPELING, Actie.INZIEN));
	}

	private void addButton(WebMarkupContainer panelContainer, List<Component> buttons, String id, MammaVisitatieOnderzoekStatus status, boolean btnVisible)
	{
		IndicatingAjaxLink<Void> button = new IndicatingAjaxLink<Void>(id)
		{
			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				AjaxCallListener myAjaxCallListener = new AjaxCallListener();
				myAjaxCallListener.onBefore("logOnAfrondenClick();");
				attributes.getAjaxCallListeners().add(myAjaxCallListener);
			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				volgende(target, status);
			}

		};
		button.setVisible(btnVisible);
		button.setOutputMarkupId(true);
		panelContainer.add(button);
		buttons.add(button);

	}

	private void volgende(AjaxRequestTarget target, MammaVisitatieOnderzoekStatus nieuweOnderzoekStatus)
	{
		kwaliteitscontroleService.wijzigOnderzoekStatus(adhocMeekijkverzoekOnderzoekModel.getObject(), nieuweOnderzoekStatus);
		((AbstractMammaBeoordelenPage) getPage()).volgendeVerslag(target);
	}

	protected void renderPanelComponents()
	{
		createButtons(panelContainer, buttons);
	}

	public void blokeerButtons(AjaxRequestTarget target)
	{
		buttons.stream().forEach(b -> b.setEnabled(false));
		target.add(buttons.toArray(new Component[] {}));
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(adhocMeekijkverzoekOnderzoekModel);
	}
}

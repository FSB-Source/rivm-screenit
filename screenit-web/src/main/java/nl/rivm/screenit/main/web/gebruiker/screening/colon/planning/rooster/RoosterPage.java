package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.filter.SecurityHeadersFilter;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.PlanningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.RoosterAantallenPerJaarPanel;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.services.ScheduleService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.PriorityHeaderItem;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.JQueryUIJavaScriptResourceReference;

public class RoosterPage extends PlanningBasePage
{
	@SpringBean
	private ScheduleService scheduleService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private final LocatieRooster locatieRooster;

	@Override
	protected void setHeaders(WebResponse response)
	{
		super.setHeaders(response);

		SecurityHeadersFilter.allowUnsafeInlineSecurityPolicy(response);
	}

	public RoosterPage()
	{
		var dialog = new BootstrapDialog("dialog");
		dialog.setOutputMarkupPlaceholderTag(true);
		add(dialog);
		var roosterAantallenPerJaarPanel = new RoosterAantallenPerJaarPanel("aantallen", ModelUtil.cRModel(ScreenitSession.get().getColoscopieCentrum()));
		roosterAantallenPerJaarPanel.setOutputMarkupId(true);
		locatieRooster = new LocatieRooster("rooster", roosterAantallenPerJaarPanel, dialog);
		locatieRooster.setOutputMarkupId(true);
		add(locatieRooster);
		add(new Label("naamInstelling", ScreenitSession.get().getColoscopieCentrum().getNaam()));

		add(roosterAantallenPerJaarPanel);

		add(new AjaxLink<Void>("nieuwBlokkade")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ColonBlokkade item = new ColonBlokkade();
				item.setRecurrence(new NoRecurrence());

				item.setTitle(ColonTijdSlotType.BLOKKADE.getTitle());

				locatieRooster.editBlokkade(target, item);
			}
		});

		add(new AjaxLink<Void>("nieuwRoosterBlock")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				var item = new RoosterItem();
				item.setRecurrence(new NoRecurrence());
				var startTime = currentDateSupplier.getLocalDateTime();
				startTime = DateUtil.roundMinutes(startTime);

				item.setStartTime(DateUtil.toUtilDate(startTime));
				item.setEndTime(DateUtil.toUtilDate(startTime.plusMinutes(ScreenitSession.get().getColoscopieCentrum().getAfspraakDefinities().get(0).getDuurAfspraakInMinuten())));

				var alleRoosterblokken = scheduleService.getAlleRoosterblokken(null);
				var scheduleSet = alleRoosterblokken.get(0);
				item.setScheduleSet(scheduleSet);
				item.setTitle(scheduleSet.getTitle());

				locatieRooster.editRoosteritem(target, item);
			}

			@Override
			public boolean isVisible()
			{
				return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_LOCATIE_ROOSTER, Actie.TOEVOEGEN);
			}
		});
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(new PriorityHeaderItem(JavaScriptHeaderItem.forReference(JQueryUIJavaScriptResourceReference.get())));
	}
}

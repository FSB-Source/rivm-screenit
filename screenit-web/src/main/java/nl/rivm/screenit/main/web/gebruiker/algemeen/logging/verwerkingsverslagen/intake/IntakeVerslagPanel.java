
package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.intake;

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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;

import nl.rivm.screenit.model.colon.IntakeMakenLogEventRegel;
import nl.rivm.screenit.model.logging.IntakeMakenLogEvent;
import nl.rivm.screenit.service.ScopeService;
import nl.topicuszorg.util.date.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class IntakeVerslagPanel extends GenericPanel<IntakeMakenLogEvent>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ScopeService scopeService;

	private int aantalGemaakteAfspraken = 0;

	private String aantalGebruikteDagen = "";

	public IntakeVerslagPanel(String id, IModel<IntakeMakenLogEvent> model, Date datumVerwerking)
	{
		super(id, new CompoundPropertyModel<>(model));

		Iterator<String> meldingen = Arrays.asList(model.getObject().getMelding().split("<br>")).iterator();
		meldingen.next();
		String melding = StringUtils.join(meldingen, "<br>");
		prepareAantalen();

		add(DateLabel.forDatePattern("datumVerwerking", Model.of(datumVerwerking), "dd-MM-yyyy HH:mm:ss"));
		add(new Label("aantalClienten"));
		add(new Label("aantalVrijesloten"));
		add(new Label("plannerResultaat"));
		add(new Label("aantalGemaakteAfspraken", aantalGemaakteAfspraken));
		add(new Label("aantalRondes"));
		add(new Label("aantalGebruikteDagen", aantalGebruikteDagen));
		add(new Label("aantalBuitenMaximaleAfstand"));

		add(new Label("melding", melding).setEscapeModelStrings(false).setVisible(StringUtils.isNotBlank(melding)));
		add(new PropertyListView<String>("exceptionStackTrace")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<String> item)
			{
				item.add(new Label("fout", item.getModelObject().replaceAll("\n", "<br>")).setEscapeModelStrings(false));
			}
		});
	}

	private void prepareAantalen()
	{
		IntakeMakenLogEvent logEvent = getModelObject();

		for (IntakeMakenLogEventRegel regel : logEvent.getRegels())
		{
			if (regel.getAfspraakId() != null)
			{
				aantalGemaakteAfspraken++;
			}
		}

		if (logEvent.getBeginTijd() != null && logEvent.getEindTijd() != null)
		{
			DateFormat df = new SimpleDateFormat("dd-MM-yyyy");
			aantalGebruikteDagen = String.format("%s (%s tot %s)", DateUtil.aantalDagenVerschil(logEvent.getBeginTijd(), logEvent.getEindTijd()),
				df.format(logEvent.getBeginTijd()), df.format(logEvent.getEindTijd()));
		}
	}
}

import { useLocation } from 'react-router-dom';
import { useState, useContext, useEffect } from 'react'
import Game from './Game'
import ProjectContext from '../ProjectContext';
import { database } from '../firebaseConfig';
import { ref, onValue, off } from 'firebase/database';

const Connect4 = () => {
    const locationData = useLocation();
    const id = parseInt(locationData.pathname.substring(locationData.pathname.lastIndexOf("/") + 1))
    const projects = useContext(ProjectContext);
    const [isGameVisible, setIsGameVisible] = useState(false);

    function findProject(projects) {
        for (let i = 0; i < projects.length; i++) {
            if (projects.at(i).id === id) {
                return projects.at(i)
            }
        }
        return null
    }
    const [record, setRecord] = useState({ Ties: 0, Losses: 0, Wins: 0 });

    useEffect(() => {
        const recordRef = ref(database, 'Connect4/Record');

        const unsubscribe = onValue(recordRef, (snapshot) => {
            if (snapshot.exists()) {
                setRecord(snapshot.val());
                console.log(snapshot.val())
            } else {
                // handle the case where there is no data at the 'Record' node
                console.log("No data available at Record path");
            }
        }, (error) => {
            // handle error
            console.error(error);
        });
        // Clean up the listener when the component unmounts
        return () => off(recordRef);
    }, []);
    const project = findProject(projects)
    return (
        <div>
            <div className='projectDetails'>
                <img className='project1' alt="" src={project.img} />
                <article className='containProject'>
                    <h1>{project.title}</h1>
                    <p className='roleDate'>{project.role}    •    {project.date}</p>
                    <p className='body'>{project.body}</p>
                    <div className='role'>
                        <h2>ROLE</h2>
                        <p>{project.role}</p>
                    </div>
                    <div className='skills'>
                        <h2>SKILLS</h2>
                        <pre>
                            <p>{project.skills}</p>
                        </pre>
                    </div>
                    <div className='collabP'>
                        <h2>COLLABORATORS</h2>
                        <pre>
                            <p>{project.collab}</p>
                        </pre>
                    </div>
                </article>
                <div className='border'></div>
            </div>

            <div className='mobilePDetails'>
                <img className='project1' alt="" src={project.img} />
                <article className='containProjectM'>
                    <h1>{project.title}</h1>
                    <p className='roleDate'>{project.role}    •    {project.date}</p>
                    <p className='body'>{project.body}</p>
                    <div className='mobilePContainer'>
                        <div className='role'>
                            <h2>ROLE</h2>
                            <p>{project.role}</p>
                        </div>
                        <div className='skills'>
                            <h2>SKILLS</h2>
                            <pre>
                                <p>{project.skills}</p>
                            </pre>
                        </div>
                        <div className='collabP'>
                            <h2>COLLABORATORS</h2>
                            <pre>
                                <p>{project.collab}</p>
                            </pre>
                        </div>
                    </div>
                </article>
                <div className='border'></div>
            </div>
            <h1 className='question'>Are you smarter than AI?</h1>
            <div onClick={() => setIsGameVisible(true)} className="box-1">
                <div className="btn btn-one">
                    <span>PLAY GAME</span>
                </div>
            </div>
            {isGameVisible && <Game onClose={() => setIsGameVisible(false)} record={record} />}
            <div className="counter">
                <h1>Wins/Losses/Ties</h1>
                <h2 className='Wins'>{record.Wins}</h2>
                <h2>/</h2>
                <h2 className='Losses'>{record.Losses}</h2>
                <h2>/</h2>
                <h2 className='Draws'>{record.Ties}</h2>
            </div>
        </div>
    );
}

export default Connect4;